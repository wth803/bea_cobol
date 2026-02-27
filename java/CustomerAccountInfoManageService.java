import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Customer Account Info Manage Service (客户账户路由管理服务).
 *
 * <p>Manages customer account routing information supporting add ({@code "01"}),
 * modify ({@code "02"}), and delete ({@code "03"}) operations.
 *
 * <p>Converted from COBOL program MGMTCRT01 (5.MgmtCustAcctInfo.cbl).
 */
public class CustomerAccountInfoManageService {

    private final Connection connection;

    public CustomerAccountInfoManageService(Connection connection) {
        this.connection = connection;
    }

    /**
     * Response object returned by {@link #manage}.
     */
    public static class ManageResponse {
        private String respCode;
        private String respMsg;
        private int recordCount;

        public String getRespCode() { return respCode; }
        public String getRespMsg() { return respMsg; }
        public int getRecordCount() { return recordCount; }
    }

    /**
     * Adds, modifies, or logically deletes a customer account routing record.
     *
     * @param tenantNo     tenant number (租户编号)
     * @param custNo       customer number (客户编号)
     * @param afsProDtNo   AFS product number (可售产品编号)
     * @param baseProDtNo  base product number (基础产品编号)
     * @param mainAcctNo   main account number (主账号)
     * @param operTypCd    operation type code: {@code "01"} add, {@code "02"} modify,
     *                     {@code "03"} delete (操作类型代码)
     * @param relaSeqNo    relation sequence number (关联序号)
     * @param routeTypCd   route type code (路由类型代码)
     * @param routeVal     route value (路由值)
     * @param operTelrNo   operator teller number (操作柜员号)
     * @return {@link ManageResponse} containing result code, message, and affected
     *         record count
     */
    public ManageResponse manage(String tenantNo, String custNo, String afsProDtNo,
            String baseProDtNo, String mainAcctNo, String operTypCd, String relaSeqNo,
            String routeTypCd, String routeVal, String operTelrNo) {

        ManageResponse response = new ManageResponse();
        response.respCode = "E99999";
        response.respMsg = "PROCESSING ERROR";
        response.recordCount = 0;

        // 1) Validate required parameters
        if (tenantNo == null || tenantNo.trim().isEmpty()) {
            response.respCode = "F20001";
            response.respMsg = "租户号不能为空";
            return response;
        }
        if (custNo == null || custNo.trim().isEmpty()) {
            response.respCode = "F20002";
            response.respMsg = "客户编号不能为空";
            return response;
        }
        if (routeVal == null || routeVal.trim().isEmpty()) {
            response.respCode = "F20003";
            response.respMsg = "路由值不能为空";
            return response;
        }
        if (routeTypCd == null || routeTypCd.trim().isEmpty()) {
            response.respCode = "F20004";
            response.respMsg = "路由类型不能为空";
            return response;
        }
        if (operTypCd == null || operTypCd.trim().isEmpty()) {
            response.respCode = "F20005";
            response.respMsg = "操作类型不能为空";
            return response;
        }

        // 2) Validate operation type code
        if (!operTypCd.equals("01") && !operTypCd.equals("02") && !operTypCd.equals("03")) {
            response.respCode = "E12196";
            response.respMsg = "非法操作标志";
            return response;
        }

        try {
            // 3) Begin transaction
            connection.setAutoCommit(false);

            int affected;
            switch (operTypCd) {
                case "01":
                    affected = addCustAcctInfo(tenantNo, custNo, afsProDtNo, baseProDtNo,
                            mainAcctNo, operTypCd, relaSeqNo, routeTypCd, routeVal, operTelrNo);
                    response.respMsg = "客户账户路由信息新增成功";
                    break;
                case "02":
                    affected = modifyCustAcctInfo(tenantNo, custNo, afsProDtNo, baseProDtNo,
                            mainAcctNo, operTypCd, relaSeqNo, routeTypCd, routeVal, operTelrNo);
                    response.respMsg = "客户账户路由信息修改成功";
                    break;
                default: // "03"
                    affected = deleteCustAcctInfo(tenantNo, custNo, relaSeqNo, routeTypCd,
                            routeVal, operTelrNo);
                    response.respMsg = "客户账户路由信息删除成功";
                    break;
            }

            // 4) Commit transaction
            connection.commit();

            response.respCode = "000000";
            response.recordCount = affected;

        } catch (DuplicateRecordException e) {
            rollback();
            response.respCode = "F20006";
            response.respMsg = "客户账户路由信息已存在";
        } catch (RecordNotFoundException e) {
            rollback();
            response.respCode = "F20007";
            response.respMsg = "客户账户路由信息不存在";
        } catch (SQLException e) {
            rollback();
            response.respCode = "E12002";
            response.respMsg = "事务提交失败";
        }

        return response;
    }

    // -------------------------------------------------------------------------
    // Private helpers
    // -------------------------------------------------------------------------

    private int addCustAcctInfo(String tenantNo, String custNo, String afsProDtNo,
            String baseProDtNo, String mainAcctNo, String operTypCd, String relaSeqNo,
            String routeTypCd, String routeVal, String operTelrNo)
            throws SQLException, DuplicateRecordException {
        if (recordExists(tenantNo, custNo, relaSeqNo, routeTypCd, routeVal)) {
            throw new DuplicateRecordException();
        }
        String sql = "INSERT INTO CUST_ACCT_INFO "
                + "(TENANT_NO, CUST_NO, AFS_PRODT_NO, BASE_PRODT_NO, MAIN_ACCT_NO, "
                + " OPER_TYP_CD, RELA_SEQ_NO, ROUTE_TYP_CD, ROUTE_VAL, VALID_FLG, "
                + " CRT_TELR_NO, UPD_TELR_NO, CRT_TM, UPD_TM) "
                + "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, '1', ?, ?, "
                + "CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, tenantNo);
            ps.setString(2, custNo);
            ps.setString(3, afsProDtNo);
            ps.setString(4, baseProDtNo);
            ps.setString(5, mainAcctNo);
            ps.setString(6, operTypCd);
            ps.setString(7, relaSeqNo);
            ps.setString(8, routeTypCd);
            ps.setString(9, routeVal);
            ps.setString(10, operTelrNo);
            ps.setString(11, operTelrNo);
            return ps.executeUpdate();
        }
    }

    private int modifyCustAcctInfo(String tenantNo, String custNo, String afsProDtNo,
            String baseProDtNo, String mainAcctNo, String operTypCd, String relaSeqNo,
            String routeTypCd, String routeVal, String operTelrNo)
            throws SQLException, RecordNotFoundException {
        if (!recordExists(tenantNo, custNo, relaSeqNo, routeTypCd, routeVal)) {
            throw new RecordNotFoundException();
        }
        String sql = "UPDATE CUST_ACCT_INFO "
                + "SET AFS_PRODT_NO = ?, BASE_PRODT_NO = ?, MAIN_ACCT_NO = ?, "
                + "    OPER_TYP_CD = ?, UPD_TELR_NO = ?, UPD_TM = CURRENT_TIMESTAMP "
                + "WHERE TENANT_NO = ? AND CUST_NO = ? AND ROUTE_TYP_CD = ? "
                + "  AND ROUTE_VAL = ? AND RELA_SEQ_NO = ? AND VALID_FLG = '1'";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, afsProDtNo);
            ps.setString(2, baseProDtNo);
            ps.setString(3, mainAcctNo);
            ps.setString(4, operTypCd);
            ps.setString(5, operTelrNo);
            ps.setString(6, tenantNo);
            ps.setString(7, custNo);
            ps.setString(8, routeTypCd);
            ps.setString(9, routeVal);
            ps.setString(10, relaSeqNo);
            return ps.executeUpdate();
        }
    }

    private int deleteCustAcctInfo(String tenantNo, String custNo, String relaSeqNo,
            String routeTypCd, String routeVal, String operTelrNo)
            throws SQLException, RecordNotFoundException {
        if (!recordExists(tenantNo, custNo, relaSeqNo, routeTypCd, routeVal)) {
            throw new RecordNotFoundException();
        }
        String sql = "UPDATE CUST_ACCT_INFO "
                + "SET VALID_FLG = '0', UPD_TELR_NO = ?, UPD_TM = CURRENT_TIMESTAMP "
                + "WHERE TENANT_NO = ? AND CUST_NO = ? AND ROUTE_TYP_CD = ? "
                + "  AND ROUTE_VAL = ? AND RELA_SEQ_NO = ? AND VALID_FLG = '1'";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, operTelrNo);
            ps.setString(2, tenantNo);
            ps.setString(3, custNo);
            ps.setString(4, routeTypCd);
            ps.setString(5, routeVal);
            ps.setString(6, relaSeqNo);
            return ps.executeUpdate();
        }
    }

    private boolean recordExists(String tenantNo, String custNo, String relaSeqNo,
            String routeTypCd, String routeVal) throws SQLException {
        String sql = "SELECT COUNT(*) FROM CUST_ACCT_INFO "
                + "WHERE TENANT_NO = ? AND CUST_NO = ? AND ROUTE_TYP_CD = ? "
                + "  AND ROUTE_VAL = ? AND RELA_SEQ_NO = ? AND VALID_FLG = '1'";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, tenantNo);
            ps.setString(2, custNo);
            ps.setString(3, routeTypCd);
            ps.setString(4, routeVal);
            ps.setString(5, relaSeqNo);
            try (ResultSet rs = ps.executeQuery()) {
                return rs.next() && rs.getInt(1) > 0;
            }
        }
    }

    private void rollback() {
        try {
            connection.rollback();
        } catch (SQLException ignored) {
        }
    }

    private static class DuplicateRecordException extends Exception {
        DuplicateRecordException() { super("Record already exists"); }
    }

    private static class RecordNotFoundException extends Exception {
        RecordNotFoundException() { super("Record not found"); }
    }
}

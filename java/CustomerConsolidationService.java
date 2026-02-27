import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Customer Consolidation Service (客户归并服务).
 *
 * <p>Implements customer consolidation by migrating account routing from the source
 * customer to the target customer.
 *
 * <p>Converted from COBOL program CUSTMRG01 (1.ConsolidationCust.cbl).
 */
public class CustomerConsolidationService {

    private final Connection connection;

    public CustomerConsolidationService(Connection connection) {
        this.connection = connection;
    }

    /**
     * Response object returned by {@link #consolidate}.
     */
    public static class ConsolidationResponse {
        private String respCode;
        private String respMsg;
        private int updateCount;

        public String getRespCode() { return respCode; }
        public String getRespMsg() { return respMsg; }
        public int getUpdateCount() { return updateCount; }
    }

    /**
     * Consolidates the source customer into the target customer by reassigning all
     * account-routing records from {@code mergeCustNo} to {@code custNo}.
     *
     * @param custNo      target customer number (并入客户号)
     * @param mergeCustNo source customer number (并出客户号)
     * @param operTelrNo  operator teller number (操作柜员号)
     * @param tenantNo    tenant number (租户编号)
     * @return {@link ConsolidationResponse} containing the result code, message, and
     *         number of updated records
     */
    public ConsolidationResponse consolidate(String custNo, String mergeCustNo,
            String operTelrNo, String tenantNo) {
        ConsolidationResponse response = new ConsolidationResponse();
        response.respCode = "E99999";
        response.respMsg = "PROCESSING ERROR";
        response.updateCount = 0;

        // 1) Validate required parameters
        if (custNo == null || custNo.trim().isEmpty()) {
            response.respCode = "F20001";
            response.respMsg = "并入客户号不能为空";
            return response;
        }
        if (mergeCustNo == null || mergeCustNo.trim().isEmpty()) {
            response.respCode = "F20002";
            response.respMsg = "并出客户号不能为空";
            return response;
        }

        try {
            // 2) Check that the target customer exists and is valid
            int targetCount = countValidCustomer(custNo, tenantNo);
            if (targetCount == 0) {
                response.respCode = "F20003";
                response.respMsg = "并入客户不存在或无效";
                return response;
            }

            // 3) Check that the source customer exists and is valid
            int sourceCount = countValidCustomer(mergeCustNo, tenantNo);
            if (sourceCount == 0) {
                response.respCode = "F20004";
                response.respMsg = "并出客户不存在或无效";
                return response;
            }

            // 4) Begin transaction
            connection.setAutoCommit(false);

            // 5) Execute consolidation – reassign routing records to the target customer
            int updateCount = updateCustAcctRouting(custNo, mergeCustNo, operTelrNo, tenantNo);

            // 6) Check for routing conflicts
            int conflictCount = countRoutingConflicts(custNo, tenantNo);

            if (conflictCount > 0) {
                response.respCode = "W20001";
                response.respMsg = "客户归并完成，但存在" + conflictCount + "条路由冲突记录";
            } else {
                response.respCode = "000000";
                response.respMsg = "客户归并成功，更新" + updateCount + "条记录";
            }
            response.updateCount = updateCount;

            // 7) Commit transaction
            connection.commit();

        } catch (SQLException e) {
            rollback();
            response.respCode = "E12002";
            response.respMsg = "客户归并更新失败";
        }

        return response;
    }

    // -------------------------------------------------------------------------
    // Private helpers
    // -------------------------------------------------------------------------

    private int countValidCustomer(String custNo, String tenantNo) throws SQLException {
        String sql = "SELECT COUNT(*) FROM CUST_ACCT_INFO "
                + "WHERE CUST_NO = ? AND TENANT_NO = ? AND VALID_FLG = '1'";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, custNo);
            ps.setString(2, tenantNo);
            try (ResultSet rs = ps.executeQuery()) {
                return rs.next() ? rs.getInt(1) : 0;
            }
        }
    }

    private int updateCustAcctRouting(String custNo, String mergeCustNo,
            String operTelrNo, String tenantNo) throws SQLException {
        String sql = "UPDATE CUST_ACCT_INFO "
                + "SET CUST_NO = ?, UPD_TELR_NO = ?, UPD_TM = CURRENT_TIMESTAMP "
                + "WHERE CUST_NO = ? AND TENANT_NO = ? AND VALID_FLG = '1'";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, custNo);
            ps.setString(2, operTelrNo);
            ps.setString(3, mergeCustNo);
            ps.setString(4, tenantNo);
            return ps.executeUpdate();
        }
    }

    private int countRoutingConflicts(String custNo, String tenantNo) throws SQLException {
        String sql = "SELECT COUNT(*) FROM CUST_ACCT_INFO cai1 "
                + "WHERE cai1.CUST_NO = ? AND cai1.TENANT_NO = ? AND cai1.VALID_FLG = '1' "
                + "AND EXISTS (SELECT 1 FROM CUST_ACCT_INFO cai2 "
                + "  WHERE cai2.CUST_NO = ? AND cai2.TENANT_NO = ? "
                + "  AND cai2.VALID_FLG = '1' "
                + "  AND cai2.ROUTE_VAL = cai1.ROUTE_VAL "
                + "  AND cai2.ROUTE_TYP_CD = cai1.ROUTE_TYP_CD "
                + "  AND cai2.RELA_SEQ_NO <> cai1.RELA_SEQ_NO)";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, custNo);
            ps.setString(2, tenantNo);
            ps.setString(3, custNo);
            ps.setString(4, tenantNo);
            try (ResultSet rs = ps.executeQuery()) {
                return rs.next() ? rs.getInt(1) : 0;
            }
        }
    }

    private void rollback() {
        try {
            connection.rollback();
        } catch (SQLException ignored) {
        }
    }
}

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.List;

/**
 * Customer Consolidation By Account Service (按账号客户归并服务).
 *
 * <p>Implements customer consolidation by updating routing records for a specified
 * account list from the source customer to the target customer.
 *
 * <p>Converted from COBOL program CUSTMRG02 (2.ConsolidationCustByAcctNo.cbl).
 */
public class CustomerConsolidationByAcctService {

    private final Connection connection;

    public CustomerConsolidationByAcctService(Connection connection) {
        this.connection = connection;
    }

    /**
     * Response object returned by {@link #consolidate}.
     */
    public static class ConsolidationByAcctResponse {
        private String respCode;
        private String respMsg;
        private int updateCount;

        public String getRespCode() { return respCode; }
        public String getRespMsg() { return respMsg; }
        public int getUpdateCount() { return updateCount; }
    }

    /**
     * Consolidates a list of accounts from the source customer into the target customer.
     *
     * @param custNo       target customer number (并入客户号)
     * @param mergeCustNo  source customer number (并出客户号)
     * @param routeTypCd   route type code (路由类型代码)
     * @param operTelrNo   operator teller number (操作柜员号)
     * @param tenantNo     tenant number (租户编号)
     * @param acctNos      list of account numbers to consolidate (账号集合)
     * @return {@link ConsolidationByAcctResponse} containing result code, message, and
     *         number of updated records
     */
    public ConsolidationByAcctResponse consolidate(String custNo, String mergeCustNo,
            String routeTypCd, String operTelrNo, String tenantNo, List<String> acctNos) {

        ConsolidationByAcctResponse response = new ConsolidationByAcctResponse();
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
        if (routeTypCd == null || routeTypCd.trim().isEmpty()) {
            response.respCode = "F20003";
            response.respMsg = "路由类型代码不能为空";
            return response;
        }
        if (acctNos == null || acctNos.isEmpty()) {
            response.respCode = "F20004";
            response.respMsg = "归并账号集合不能为空";
            return response;
        }

        try {
            // 2) Begin transaction
            connection.setAutoCommit(false);

            // 3) Update each account routing record
            int totalUpdated = 0;
            for (String acctNo : acctNos) {
                int updated = updateRoutingRecord(custNo, mergeCustNo, routeTypCd,
                        operTelrNo, tenantNo, acctNo);
                totalUpdated += updated;
            }

            // 4) Commit transaction
            connection.commit();

            response.respCode = "000000";
            response.respMsg = "客户归并成功，更新" + totalUpdated + "条记录";
            response.updateCount = totalUpdated;

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

    private int updateRoutingRecord(String custNo, String mergeCustNo, String routeTypCd,
            String operTelrNo, String tenantNo, String acctNo) throws SQLException {
        String sql = "UPDATE CUST_ACCT_INFO "
                + "SET CUST_NO = ?, UPD_TELR_NO = ?, UPD_TM = CURRENT_TIMESTAMP "
                + "WHERE ROUTE_VAL = ? AND ROUTE_TYP_CD = ? "
                + "  AND CUST_NO = ? AND TENANT_NO = ? AND VALID_FLG = '1'";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, custNo);
            ps.setString(2, operTelrNo);
            ps.setString(3, acctNo);
            ps.setString(4, routeTypCd);
            ps.setString(5, mergeCustNo);
            ps.setString(6, tenantNo);
            return ps.executeUpdate();
        }
    }

    private void rollback() {
        try {
            connection.rollback();
        } catch (SQLException ignored) {
        }
    }
}

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Query Overseas Cash Withdrawal Control Flag Service (境外取现控制标志查询服务).
 *
 * <p>Implements overseas cash withdrawal blacklist flag query by customer number.
 * First retrieves the customer's credential information, then checks the overseas
 * cash withdrawal blacklist table.
 *
 * <p>Converted from COBOL program QURYOVSCASHWITHDRRECTRFLG
 * (11.QuryOvsCashWithdrReCtrFlg.cbl).
 */
public class QueryOverseasCashWithdrawalControlFlagService {

    private final Connection connection;

    public QueryOverseasCashWithdrawalControlFlagService(Connection connection) {
        this.connection = connection;
    }

    /**
     * Response object returned by {@link #query}.
     */
    public static class QueryOvsCtrlFlagResponse {
        private int returnCode;
        private String returnMessage;
        /** {@code "0"} when the customer is on the blacklist, {@code "N"} otherwise. */
        private String blacklistCustFlg;

        public int getReturnCode() { return returnCode; }
        public String getReturnMessage() { return returnMessage; }
        public String getBlacklistCustFlg() { return blacklistCustFlg; }
    }

    /**
     * Queries the overseas cash withdrawal blacklist control flag for the given customer.
     *
     * @param custNo customer number (客户编号)
     * @return {@link QueryOvsCtrlFlagResponse} containing result code, message, and the
     *         blacklist flag
     */
    public QueryOvsCtrlFlagResponse query(String custNo) {
        QueryOvsCtrlFlagResponse response = new QueryOvsCtrlFlagResponse();
        response.returnCode = 0;
        response.blacklistCustFlg = "N";

        // 1) Validate required parameter
        if (custNo == null || custNo.trim().isEmpty()) {
            response.returnCode = 1001;
            response.returnMessage = "客户编号不能为空";
            return response;
        }

        try {
            // 2) Look up the customer's credential information
            CustomerCredential credential = findCustomerCredential(custNo);
            if (credential == null) {
                response.returnCode = 20000;
                response.returnMessage = "未找到客户基本信息";
                return response;
            }

            // 3) Check whether the customer is on the overseas cash withdrawal blacklist
            boolean onBlacklist = isOnBlacklist(credential.crtfNo, credential.crtfTypCd);
            response.blacklistCustFlg = onBlacklist ? "0" : "N";
            response.returnCode = 0;
            response.returnMessage = "查询成功";

        } catch (SQLException e) {
            response.returnCode = 9999;
            response.returnMessage = "数据库查询失败";
        }

        return response;
    }

    // -------------------------------------------------------------------------
    // Private helpers
    // -------------------------------------------------------------------------

    private static class CustomerCredential {
        String crtfNo;
        String crtfTypCd;
    }

    private CustomerCredential findCustomerCredential(String custNo) throws SQLException {
        String sql = "SELECT CRTF_NO, CRTF_TYP_CD FROM CUSTOMER_BASIC_INFO "
                + "WHERE CUST_NO = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, custNo);
            try (ResultSet rs = ps.executeQuery()) {
                if (rs.next()) {
                    CustomerCredential cred = new CustomerCredential();
                    cred.crtfNo = rs.getString("CRTF_NO");
                    cred.crtfTypCd = rs.getString("CRTF_TYP_CD");
                    return cred;
                }
            }
        }
        return null;
    }

    private boolean isOnBlacklist(String crtfNo, String crtfTypCd) throws SQLException {
        String sql = "SELECT COUNT(*) FROM OVS_CASH_WITHDR_BLK "
                + "WHERE CRTF_NO = ? AND CRTF_TYP_CD = ? AND VALID_FLG = '1'";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, crtfNo);
            ps.setString(2, crtfTypCd);
            try (ResultSet rs = ps.executeQuery()) {
                return rs.next() && rs.getInt(1) > 0;
            }
        }
    }
}

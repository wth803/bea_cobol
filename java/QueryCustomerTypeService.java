import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Query Customer Type Service (客户类型查询服务).
 *
 * <p>Implements customer type code query by customer number.
 *
 * <p>Converted from COBOL program QURYCUSTTYPE (10.QuryCustType.cbl).
 */
public class QueryCustomerTypeService {

    private final Connection connection;

    public QueryCustomerTypeService(Connection connection) {
        this.connection = connection;
    }

    /**
     * Response object returned by {@link #query}.
     */
    public static class QueryCustomerTypeResponse {
        private int returnCode;
        private String returnMessage;
        private String custTypCd;

        public int getReturnCode() { return returnCode; }
        public String getReturnMessage() { return returnMessage; }
        public String getCustTypCd() { return custTypCd; }
    }

    /**
     * Queries the customer type code for the given customer number.
     *
     * @param custNo customer number (客户编号)
     * @return {@link QueryCustomerTypeResponse} containing result code, message, and
     *         the customer type code
     */
    public QueryCustomerTypeResponse query(String custNo) {
        QueryCustomerTypeResponse response = new QueryCustomerTypeResponse();
        response.returnCode = 0;

        // 1) Validate required parameter
        if (custNo == null || custNo.trim().isEmpty()) {
            response.returnCode = 1001;
            response.returnMessage = "客户编号不能为空";
            return response;
        }

        try {
            // 2) Look up the customer type code
            String custTypCd = findCustomerType(custNo);
            if (custTypCd == null) {
                response.returnCode = 20000;
                response.returnMessage = "未找到客户基本信息";
            } else {
                response.returnCode = 0;
                response.returnMessage = "查询成功";
                response.custTypCd = custTypCd;
            }

        } catch (SQLException e) {
            response.returnCode = 9999;
            response.returnMessage = "数据库查询失败";
        }

        return response;
    }

    // -------------------------------------------------------------------------
    // Private helpers
    // -------------------------------------------------------------------------

    private String findCustomerType(String custNo) throws SQLException {
        String sql = "SELECT CUST_TYP_CD FROM CUSTOMER_BASIC_INFO WHERE CUST_NO = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, custNo);
            try (ResultSet rs = ps.executeQuery()) {
                return rs.next() ? rs.getString("CUST_TYP_CD") : null;
            }
        }
    }
}

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Query Personal Customer Risk Level Service (对私客户风险等级查询服务).
 *
 * <p>Implements personal customer risk level and risk assessment result query by
 * customer number. First retrieves customer basic information, then queries the
 * risk-level table.
 *
 * <p>Converted from COBOL program QURYPERCUSTRISKLEVEL (15.QuryPerCustRiskLevel.cbl).
 */
public class QueryPersonalCustomerRiskLevelService {

    private final Connection connection;

    public QueryPersonalCustomerRiskLevelService(Connection connection) {
        this.connection = connection;
    }

    /**
     * Response object returned by {@link #query}.
     */
    public static class QueryRiskLevelResponse {
        private int returnCode;
        private String returnMessage;
        private String custAttnExttCd;
        private String custNo;
        private String custTypCd;
        private String evaltAcrdgasComnt;
        private String evaltDt;
        private String relsDt;
        private String relsOrIsuOrgNo;

        public int getReturnCode() { return returnCode; }
        public String getReturnMessage() { return returnMessage; }
        public String getCustAttnExttCd() { return custAttnExttCd; }
        public String getCustNo() { return custNo; }
        public String getCustTypCd() { return custTypCd; }
        public String getEvaltAcrdgasComnt() { return evaltAcrdgasComnt; }
        public String getEvaltDt() { return evaltDt; }
        public String getRelsDt() { return relsDt; }
        public String getRelsOrIsuOrgNo() { return relsOrIsuOrgNo; }
    }

    /**
     * Queries risk level information for the given customer.
     *
     * @param custNo customer number (客户编号)
     * @return {@link QueryRiskLevelResponse} with result code, message, and risk-level
     *         data
     */
    public QueryRiskLevelResponse query(String custNo) {
        QueryRiskLevelResponse response = new QueryRiskLevelResponse();
        response.returnCode = 0;

        // 1) Validate required parameter
        if (custNo == null || custNo.trim().isEmpty()) {
            response.returnCode = 1001;
            response.returnMessage = "客户编号不能为空";
            return response;
        }

        try {
            // 2) Look up the customer's basic information
            CustomerBasicSummary summary = findCustomerBasic(custNo);
            if (summary == null) {
                response.returnCode = 1002;
                response.returnMessage = "未找到客户基本信息";
                return response;
            }

            // 3) Query risk level information
            boolean riskFound = queryRiskInfo(summary.custNo, summary.custTypCd, response);
            if (!riskFound) {
                response.returnCode = 1003;
                response.returnMessage = "未找到客户风险等级信息";
                return response;
            }

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

    private static class CustomerBasicSummary {
        String custNo;
        String custTypCd;
    }

    private CustomerBasicSummary findCustomerBasic(String custNo) throws SQLException {
        String sql = "SELECT CUST_NO, CUST_TYP_CD FROM CUSTOMER_BASIC_INFO "
                + "WHERE CUST_NO = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, custNo);
            try (ResultSet rs = ps.executeQuery()) {
                if (rs.next()) {
                    CustomerBasicSummary summary = new CustomerBasicSummary();
                    summary.custNo = rs.getString("CUST_NO");
                    summary.custTypCd = rs.getString("CUST_TYP_CD");
                    return summary;
                }
            }
        }
        return null;
    }

    private boolean queryRiskInfo(String custNo, String custTypCd,
            QueryRiskLevelResponse response) throws SQLException {
        String sql = "SELECT CUST_ATTN_CD, CUST_NO, CUST_TYP_CD, EVALT_ACRDGAS_COMNT, "
                + "EVALT_DT, RELS_DT, RELS_OR_ISU_ORG_NO "
                + "FROM CUSTOMER_RISK_INFO WHERE CUST_NO = ? AND CUST_TYP_CD = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, custNo);
            ps.setString(2, custTypCd);
            try (ResultSet rs = ps.executeQuery()) {
                if (rs.next()) {
                    response.custAttnExttCd = rs.getString("CUST_ATTN_CD");
                    response.custNo = rs.getString("CUST_NO");
                    response.custTypCd = rs.getString("CUST_TYP_CD");
                    response.evaltAcrdgasComnt = rs.getString("EVALT_ACRDGAS_COMNT");
                    response.evaltDt = rs.getString("EVALT_DT");
                    response.relsDt = rs.getString("RELS_DT");
                    response.relsOrIsuOrgNo = rs.getString("RELS_OR_ISU_ORG_NO");
                    return true;
                }
            }
        }
        return false;
    }
}

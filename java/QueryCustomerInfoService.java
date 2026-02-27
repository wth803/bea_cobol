import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Query Customer Info Service (客户基本信息查询服务).
 *
 * <p>Implements customer basic information query combining customer profile and
 * risk-level information.
 *
 * <p>Converted from COBOL program QURYCUSTINFO (9.QuryCustInfo.cbl).
 */
public class QueryCustomerInfoService {

    private final Connection connection;

    public QueryCustomerInfoService(Connection connection) {
        this.connection = connection;
    }

    /**
     * Response object returned by {@link #query}.
     */
    public static class QueryCustomerInfoResponse {
        private int returnCode;
        private String returnMessage;
        private String crtfMatrDt;
        private String crtfNo;
        private String crtfTypCd;
        private String custAttnExttCd;
        private String custNm;
        private String custNo;
        private String custTypCd;

        public int getReturnCode() { return returnCode; }
        public String getReturnMessage() { return returnMessage; }
        public String getCrtfMatrDt() { return crtfMatrDt; }
        public String getCrtfNo() { return crtfNo; }
        public String getCrtfTypCd() { return crtfTypCd; }
        public String getCustAttnExttCd() { return custAttnExttCd; }
        public String getCustNm() { return custNm; }
        public String getCustNo() { return custNo; }
        public String getCustTypCd() { return custTypCd; }
    }

    /**
     * Queries customer basic information and risk level by one or more search criteria.
     * At least one of {@code custNo}/{@code custNm} or the combination of
     * {@code crtfTypCd}+{@code crtfNo} must be supplied.
     *
     * @param custNo    customer number (客户编号), optional
     * @param custNm    customer name (客户名称), optional
     * @param crtfNo    credential number (证件号码), optional
     * @param crtfTypCd credential type code (证件类型), required when {@code custNo}
     *                  is blank
     * @return {@link QueryCustomerInfoResponse} containing result code, message, and
     *         customer data
     */
    public QueryCustomerInfoResponse query(String custNo, String custNm, String crtfNo,
            String crtfTypCd) {
        QueryCustomerInfoResponse response = new QueryCustomerInfoResponse();
        response.returnCode = 0;

        // 1) Validate input parameters
        boolean noCustNo = custNo == null || custNo.trim().isEmpty();
        if (noCustNo) {
            if (crtfTypCd == null || crtfTypCd.trim().isEmpty()) {
                response.returnCode = 20004;
                response.returnMessage = "证件类型输入为空";
                return response;
            }
            if (crtfNo == null || crtfNo.trim().isEmpty()) {
                response.returnCode = 20005;
                response.returnMessage = "证件号码输入为空";
                return response;
            }
        }

        try {
            // 2) Query customer basic information
            BasicCustomerInfo basic = queryBasicInfo(custNo, custNm, crtfNo, crtfTypCd);
            if (basic == null) {
                response.returnCode = 20000;
                response.returnMessage = "未找到客户基本信息";
                return response;
            }

            // 3) Query risk level information
            String riskAttnCd = queryRiskLevel(basic.custNo, basic.custTypCd);

            // 4) Populate response
            response.returnCode = 0;
            response.returnMessage = "查询成功";
            response.custNo = basic.custNo;
            response.custNm = basic.custNm;
            response.crtfNo = basic.crtfNo;
            response.crtfTypCd = basic.crtfTypCd;
            response.crtfMatrDt = basic.crtfMatrDt;
            response.custTypCd = basic.custTypCd;
            response.custAttnExttCd = riskAttnCd;

        } catch (SQLException e) {
            response.returnCode = 9999;
            response.returnMessage = "数据库查询失败";
        }

        return response;
    }

    // -------------------------------------------------------------------------
    // Private helpers
    // -------------------------------------------------------------------------

    private static class BasicCustomerInfo {
        String custNo;
        String custTypCd;
        String custNm;
        String crtfNo;
        String crtfTypCd;
        String crtfMatrDt;
    }

    private BasicCustomerInfo queryBasicInfo(String custNo, String custNm, String crtfNo,
            String crtfTypCd) throws SQLException {
        StringBuilder sql = new StringBuilder(
                "SELECT CUST_NO, CUST_TYP_CD, CUST_NM, CRTF_NO, CRTF_TYP_CD, CRTF_MATR_DT "
                + "FROM CUSTOMER_BASIC_INFO WHERE 1=1");

        if (custNo != null && !custNo.trim().isEmpty()) {
            sql.append(" AND CUST_NO = ?");
        }
        if (custNm != null && !custNm.trim().isEmpty()) {
            sql.append(" AND CUST_NM = ?");
        }
        if (crtfNo != null && !crtfNo.trim().isEmpty()) {
            sql.append(" AND CRTF_NO = ?");
        }
        if (crtfTypCd != null && !crtfTypCd.trim().isEmpty()) {
            sql.append(" AND CRTF_TYP_CD = ?");
        }

        try (PreparedStatement ps = connection.prepareStatement(sql.toString())) {
            int idx = 1;
            if (custNo != null && !custNo.trim().isEmpty()) {
                ps.setString(idx++, custNo);
            }
            if (custNm != null && !custNm.trim().isEmpty()) {
                ps.setString(idx++, custNm);
            }
            if (crtfNo != null && !crtfNo.trim().isEmpty()) {
                ps.setString(idx++, crtfNo);
            }
            if (crtfTypCd != null && !crtfTypCd.trim().isEmpty()) {
                ps.setString(idx, crtfTypCd);
            }

            try (ResultSet rs = ps.executeQuery()) {
                if (rs.next()) {
                    BasicCustomerInfo info = new BasicCustomerInfo();
                    info.custNo = rs.getString("CUST_NO");
                    info.custTypCd = rs.getString("CUST_TYP_CD");
                    info.custNm = rs.getString("CUST_NM");
                    info.crtfNo = rs.getString("CRTF_NO");
                    info.crtfTypCd = rs.getString("CRTF_TYP_CD");
                    info.crtfMatrDt = rs.getString("CRTF_MATR_DT");
                    return info;
                }
            }
        }
        return null;
    }

    private String queryRiskLevel(String custNo, String custTypCd) throws SQLException {
        String sql = "SELECT CUST_ATTN_CD FROM CUSTOMER_RISK_INFO "
                + "WHERE CUST_NO = ? AND CUST_TYP_CD = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, custNo);
            ps.setString(2, custTypCd);
            try (ResultSet rs = ps.executeQuery()) {
                return rs.next() ? rs.getString("CUST_ATTN_CD") : null;
            }
        }
    }
}

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Query Personal Customer Info By Customer Number Service
 * (按客户号查询个人客户信息服务).
 *
 * <p>Implements comprehensive personal customer information query by customer number,
 * combining customer basic information and personal customer extended information.
 *
 * <p>Converted from COBOL program QURYPERCUSTINFOBYCUSTNO
 * (13.QuryPerCustInfoByCustNo.cbl).
 */
public class QueryPersonalCustomerInfoByCustNoService {

    private final Connection connection;

    public QueryPersonalCustomerInfoByCustNoService(Connection connection) {
        this.connection = connection;
    }

    /**
     * Response object returned by {@link #query}.
     */
    public static class QueryPersonalCustomerInfoResponse {
        private int returnCode;
        private String returnMessage;
        private String addr;
        private String adminCmprmntCd;
        private String careerTypCd;
        private String crtfMatrDt;
        private String crtfNo;
        private String crtfTypCd;
        private String custAttnExttCd;
        private String custNm;
        private String custNo;
        private String domOversFLgCd;
        private String emplyFlg;
        private String ethnicCd;
        private String genderCd;
        private String idCardTypCd;
        private String rsvdMobileNo;
        private String spsCrtfNo;
        private String spsCrtfTypCd;
        private String spsName;
        private String spsTelNo;
        private String stateAndRgnCd;

        public int getReturnCode() { return returnCode; }
        public String getReturnMessage() { return returnMessage; }
        public String getAddr() { return addr; }
        public String getAdminCmprmntCd() { return adminCmprmntCd; }
        public String getCareerTypCd() { return careerTypCd; }
        public String getCrtfMatrDt() { return crtfMatrDt; }
        public String getCrtfNo() { return crtfNo; }
        public String getCrtfTypCd() { return crtfTypCd; }
        public String getCustAttnExttCd() { return custAttnExttCd; }
        public String getCustNm() { return custNm; }
        public String getCustNo() { return custNo; }
        public String getDomOversFLgCd() { return domOversFLgCd; }
        public String getEmplyFlg() { return emplyFlg; }
        public String getEthnicCd() { return ethnicCd; }
        public String getGenderCd() { return genderCd; }
        public String getIdCardTypCd() { return idCardTypCd; }
        public String getRsvdMobileNo() { return rsvdMobileNo; }
        public String getSpsCrtfNo() { return spsCrtfNo; }
        public String getSpsCrtfTypCd() { return spsCrtfTypCd; }
        public String getSpsName() { return spsName; }
        public String getSpsTelNo() { return spsTelNo; }
        public String getStateAndRgnCd() { return stateAndRgnCd; }
    }

    /**
     * Queries comprehensive personal customer information by customer number.
     *
     * @param custNo customer number (客户编号)
     * @return {@link QueryPersonalCustomerInfoResponse} with result code, message, and
     *         all available customer fields
     */
    public QueryPersonalCustomerInfoResponse query(String custNo) {
        QueryPersonalCustomerInfoResponse response = new QueryPersonalCustomerInfoResponse();
        response.returnCode = 0;

        // 1) Validate required parameter
        if (custNo == null || custNo.trim().isEmpty()) {
            response.returnCode = 1001;
            response.returnMessage = "客户编号不能为空";
            return response;
        }

        try {
            // 2) Query customer basic information
            boolean basicFound = queryBasicInfo(custNo, response);
            if (!basicFound) {
                response.returnCode = 20000;
                response.returnMessage = "未找到客户基本信息";
                return response;
            }

            // 3) Query personal customer extended information
            boolean personalFound = queryPersonalInfo(custNo, response);
            if (!personalFound) {
                response.returnCode = 20001;
                response.returnMessage = "未找到个人客户信息";
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

    private boolean queryBasicInfo(String custNo, QueryPersonalCustomerInfoResponse resp)
            throws SQLException {
        String sql = "SELECT CUST_NO, CUST_NM, GENDER_CD, CRTF_NO, CRTF_TYP_CD, "
                + "CRTF_MATR_DT, DOM_OVERS_FLG_CD, STATE_AND_RGN_CD, ADDR, "
                + "RSVD_MOBILE_NO, EMPLY_FLG "
                + "FROM CUSTOMER_BASIC_INFO WHERE CUST_NO = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, custNo);
            try (ResultSet rs = ps.executeQuery()) {
                if (rs.next()) {
                    resp.custNo = rs.getString("CUST_NO");
                    resp.custNm = rs.getString("CUST_NM");
                    resp.genderCd = rs.getString("GENDER_CD");
                    resp.crtfNo = rs.getString("CRTF_NO");
                    resp.crtfTypCd = rs.getString("CRTF_TYP_CD");
                    resp.crtfMatrDt = rs.getString("CRTF_MATR_DT");
                    resp.domOversFLgCd = rs.getString("DOM_OVERS_FLG_CD");
                    resp.stateAndRgnCd = rs.getString("STATE_AND_RGN_CD");
                    resp.addr = rs.getString("ADDR");
                    resp.rsvdMobileNo = rs.getString("RSVD_MOBILE_NO");
                    resp.emplyFlg = rs.getString("EMPLY_FLG");
                    return true;
                }
            }
        }
        return false;
    }

    private boolean queryPersonalInfo(String custNo, QueryPersonalCustomerInfoResponse resp)
            throws SQLException {
        String sql = "SELECT ADMIN_CMPRMNT_CD, CAREER_TYP_CD, ETHNIC_CD, SPS_NAME, "
                + "SPS_CRTF_TYP_CD, SPS_CRTF_NO, SPS_TEL_NO "
                + "FROM PERSONAL_CUSTOMER_INFO WHERE CUST_NO = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, custNo);
            try (ResultSet rs = ps.executeQuery()) {
                if (rs.next()) {
                    resp.adminCmprmntCd = rs.getString("ADMIN_CMPRMNT_CD");
                    resp.careerTypCd = rs.getString("CAREER_TYP_CD");
                    resp.ethnicCd = rs.getString("ETHNIC_CD");
                    resp.spsName = rs.getString("SPS_NAME");
                    resp.spsCrtfTypCd = rs.getString("SPS_CRTF_TYP_CD");
                    resp.spsCrtfNo = rs.getString("SPS_CRTF_NO");
                    resp.spsTelNo = rs.getString("SPS_TEL_NO");
                    // Map credential type code to ID-card type code
                    resp.idCardTypCd = resp.crtfTypCd;
                    return true;
                }
            }
        }
        return false;
    }
}

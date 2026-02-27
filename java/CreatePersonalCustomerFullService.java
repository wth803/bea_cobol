import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

/**
 * Create Personal Customer Full Service – full version (个人客户开立服务（完整版）).
 *
 * <p>Implements full personal customer creation with comprehensive field validation
 * and deduplication.
 *
 * <p>Converted from COBOL program CRTPERC02 (4.CrtPerCustInfo.cbl).
 */
public class CreatePersonalCustomerFullService {

    private static final String DEFAULT_TENANT_NO = "001";

    private final Connection connection;

    public CreatePersonalCustomerFullService(Connection connection) {
        this.connection = connection;
    }

    /**
     * Response object returned by {@link #createCustomer}.
     */
    public static class CreateCustomerFullResponse {
        private String respCode;
        private String respMsg;
        private String custNo;

        public String getRespCode() { return respCode; }
        public String getRespMsg() { return respMsg; }
        public String getCustNo() { return custNo; }
    }

    /**
     * Request object containing all fields required to create a new personal customer.
     */
    public static class CreateCustomerRequest {
        private String crtfTypCd;
        private String crtfNo;
        private String custNm;
        private String genderCd;
        private String stateRgnCd;
        private String crtfGrantDt;
        private String crtfMatrDt;
        private String careerTypCd;
        private String addr;
        private String rsvdMobileNo;
        private String adminCmprmntCd;
        private String emplyFlg;
        private String passCrtfNo;
        private int renewalTms;
        private String operTelrNo;

        public void setCrtfTypCd(String crtfTypCd) { this.crtfTypCd = crtfTypCd; }
        public void setCrtfNo(String crtfNo) { this.crtfNo = crtfNo; }
        public void setCustNm(String custNm) { this.custNm = custNm; }
        public void setGenderCd(String genderCd) { this.genderCd = genderCd; }
        public void setStateRgnCd(String stateRgnCd) { this.stateRgnCd = stateRgnCd; }
        public void setCrtfGrantDt(String crtfGrantDt) { this.crtfGrantDt = crtfGrantDt; }
        public void setCrtfMatrDt(String crtfMatrDt) { this.crtfMatrDt = crtfMatrDt; }
        public void setCareerTypCd(String careerTypCd) { this.careerTypCd = careerTypCd; }
        public void setAddr(String addr) { this.addr = addr; }
        public void setRsvdMobileNo(String rsvdMobileNo) { this.rsvdMobileNo = rsvdMobileNo; }
        public void setAdminCmprmntCd(String adminCmprmntCd) { this.adminCmprmntCd = adminCmprmntCd; }
        public void setEmplyFlg(String emplyFlg) { this.emplyFlg = emplyFlg; }
        public void setPassCrtfNo(String passCrtfNo) { this.passCrtfNo = passCrtfNo; }
        public void setRenewalTms(int renewalTms) { this.renewalTms = renewalTms; }
        public void setOperTelrNo(String operTelrNo) { this.operTelrNo = operTelrNo; }
    }

    /**
     * Creates a new personal customer with full field validation.
     *
     * @param request all input fields for customer creation
     * @return {@link CreateCustomerFullResponse} containing result code, message, and
     *         the generated customer number
     */
    public CreateCustomerFullResponse createCustomer(CreateCustomerRequest request) {
        CreateCustomerFullResponse response = new CreateCustomerFullResponse();
        response.respCode = "E99999";
        response.respMsg = "PROCESSING ERROR";

        // 1) Validate all required fields
        String validationError = validateRequiredFields(request);
        if (validationError != null) {
            String[] parts = validationError.split("\\|");
            response.respCode = parts[0];
            response.respMsg = parts[1];
            return response;
        }

        try {
            // 2) Check whether the customer already exists
            boolean exists = customerExists(request.crtfTypCd, request.crtfNo);
            if (exists) {
                response.respCode = "F20006";
                response.respMsg = "客户已存在";
                return response;
            }

            // 3) Begin transaction
            connection.setAutoCommit(false);

            // 4) Generate a new customer number
            String newCustNo = generateCustNo();

            // 5) Insert customer basic information
            insertCustomerBasicInfo(newCustNo, request);

            // 6) Insert personal customer information
            insertPersonalCustomerInfo(newCustNo, request);

            // 7) Commit transaction
            connection.commit();

            response.respCode = "000000";
            response.respMsg = "客户信息开立成功";
            response.custNo = newCustNo;

        } catch (SQLException e) {
            rollback();
            response.respCode = "E12002";
            response.respMsg = "插入客户基本信息失败";
        }

        return response;
    }

    // -------------------------------------------------------------------------
    // Private helpers
    // -------------------------------------------------------------------------

    /**
     * Returns {@code null} when all fields are valid, or a pipe-delimited
     * {@code "errorCode|errorMessage"} string when a required field is missing.
     */
    private String validateRequiredFields(CreateCustomerRequest req) {
        if (req.crtfTypCd == null || req.crtfTypCd.trim().isEmpty()) {
            return "F20001|证件类型代码不能为空";
        }
        if (req.crtfNo == null || req.crtfNo.trim().isEmpty()) {
            return "F20002|证件号码不能为空";
        }
        if (req.custNm == null || req.custNm.trim().isEmpty()) {
            return "F20003|客户名称不能为空";
        }
        if (req.genderCd == null || req.genderCd.trim().isEmpty()) {
            return "F20004|性别代码不能为空";
        }
        if (req.stateRgnCd == null || req.stateRgnCd.trim().isEmpty()) {
            return "F20005|国家和地区代码不能为空";
        }
        if (req.crtfGrantDt == null || req.crtfGrantDt.trim().isEmpty()) {
            return "F20006|证件发放日期不能为空";
        }
        if (req.crtfMatrDt == null || req.crtfMatrDt.trim().isEmpty()) {
            return "F20007|证件到期日期不能为空";
        }
        if (req.careerTypCd == null || req.careerTypCd.trim().isEmpty()) {
            return "F20008|职业类型代码不能为空";
        }
        if (req.addr == null || req.addr.trim().isEmpty()) {
            return "F20009|地址不能为空";
        }
        if (req.rsvdMobileNo == null || req.rsvdMobileNo.trim().isEmpty()) {
            return "F20010|预留手机号码不能为空";
        }
        return null;
    }

    private boolean customerExists(String crtfTypCd, String crtfNo) throws SQLException {
        String sql = "SELECT COUNT(*) FROM CUSTOMER_BASIC_INFO "
                + "WHERE CRTF_TYP_CD = ? AND CRTF_NO = ? AND VALID_FLG = '1'";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, crtfTypCd);
            ps.setString(2, crtfNo);
            try (ResultSet rs = ps.executeQuery()) {
                return rs.next() && rs.getInt(1) > 0;
            }
        }
    }

    private String generateCustNo() throws SQLException {
        String sql = "SELECT 'CUST' || LPAD(NEXTVAL FOR CUST_NO_SEQ, 6, '0') "
                + "FROM SYSIBM.SYSDUMMY1";
        try (PreparedStatement ps = connection.prepareStatement(sql);
             ResultSet rs = ps.executeQuery()) {
            if (rs.next()) {
                return rs.getString(1);
            }
        }
        String ts = LocalDate.now().format(DateTimeFormatter.ofPattern("yyMMdd"))
                + System.currentTimeMillis() % 1_000_000L;
        return "CUST" + ts.substring(0, 6);
    }

    private void insertCustomerBasicInfo(String custNo, CreateCustomerRequest req)
            throws SQLException {
        String sql = "INSERT INTO CUSTOMER_BASIC_INFO "
                + "(TENANT_NO, CUST_NO, CUST_TYP_CD, CUST_LVL_CD, CRTF_TYP_CD, CRTF_NO, "
                + " CUST_NM, CRTF_GRANT_DT, CRTF_MATR_DT, STATE_AND_RGN_CD, ADDR, "
                + " RSVD_MOBILE_NO, EMPLY_FLG, VALID_FLG, CRT_TELR_NO, UPD_TELR_NO, "
                + " CRT_TM, UPD_TM) "
                + "VALUES (?, ?, '0', '1', ?, ?, ?, ?, ?, ?, ?, ?, ?, '1', ?, ?, "
                + "CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, DEFAULT_TENANT_NO);
            ps.setString(2, custNo);
            ps.setString(3, req.crtfTypCd);
            ps.setString(4, req.crtfNo);
            ps.setString(5, req.custNm);
            ps.setString(6, req.crtfGrantDt);
            ps.setString(7, req.crtfMatrDt);
            ps.setString(8, req.stateRgnCd);
            ps.setString(9, req.addr);
            ps.setString(10, req.rsvdMobileNo);
            ps.setString(11, req.emplyFlg);
            ps.setString(12, req.operTelrNo);
            ps.setString(13, req.operTelrNo);
            ps.executeUpdate();
        }
    }

    private void insertPersonalCustomerInfo(String custNo, CreateCustomerRequest req)
            throws SQLException {
        String sql = "INSERT INTO PERSONAL_CUSTOMER_INFO "
                + "(TENANT_NO, CUST_NO, GENDER_CD, CAREER_TYP_CD, ADMIN_CMPRMNT_CD, "
                + " VALID_FLG, CRT_TELR_NO, UPD_TELR_NO, CRT_TM, UPD_TM) "
                + "VALUES (?, ?, ?, ?, ?, '1', ?, ?, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, DEFAULT_TENANT_NO);
            ps.setString(2, custNo);
            ps.setString(3, req.genderCd);
            ps.setString(4, req.careerTypCd);
            ps.setString(5, req.adminCmprmntCd);
            ps.setString(6, req.operTelrNo);
            ps.setString(7, req.operTelrNo);
            ps.executeUpdate();
        }
    }

    private void rollback() {
        try {
            connection.rollback();
        } catch (SQLException ignored) {
        }
    }
}

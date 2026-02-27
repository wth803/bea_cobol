import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Personal Customer Info Manage Service (个人客户信息维护服务).
 *
 * <p>Manages personal customer basic and extended information updates with
 * partial-update (field-level) support. Only non-blank input fields are written
 * to the database; existing values are preserved for fields left empty.
 *
 * <p>Converted from COBOL program MGMT-PER-CUST-INFO (6.MgmtPerCustlnfo.cbl).
 */
public class PersonalCustomerInfoManageService {

    private final Connection connection;

    public PersonalCustomerInfoManageService(Connection connection) {
        this.connection = connection;
    }

    /**
     * Response object returned by {@link #updateCustomer}.
     */
    public static class UpdateResponse {
        private String respCode;
        private String respMsg;

        public String getRespCode() { return respCode; }
        public String getRespMsg() { return respMsg; }
    }

    /**
     * Request object for personal customer update. All fields are optional; only
     * non-null, non-blank fields are applied to the database.
     */
    public static class UpdateCustomerRequest {
        private String custNo;
        private String custNm;
        private String custEngNm;
        private String custLvlCd;
        private String mobileNo;
        private String eMail;
        private String crtfTypCd;
        private String crtfNo;
        private String crtfMatrDt;
        private String addr;
        private String housDrgstAddr;
        private String genderCd;
        private String marrgSituationCd;
        private String birthDt;
        private String careerTypCd;
        private String stateRgnCd;
        private String domOversFLgCd;
        private String idcardTypCd;
        private String emplyFlg;
        private String srhdFlg;
        private String spsName;
        private String spsEngName;
        private String spsCrtfTypCd;
        private String spsCrtfNo;
        private String spsTelNo;
        private String workunitNm;
        private String workunitAddr;
        private String adminCmprmntCd;

        public void setCustNo(String custNo) { this.custNo = custNo; }
        public void setCustNm(String custNm) { this.custNm = custNm; }
        public void setCustEngNm(String custEngNm) { this.custEngNm = custEngNm; }
        public void setCustLvlCd(String custLvlCd) { this.custLvlCd = custLvlCd; }
        public void setMobileNo(String mobileNo) { this.mobileNo = mobileNo; }
        public void setEMail(String eMail) { this.eMail = eMail; }
        public void setCrtfTypCd(String crtfTypCd) { this.crtfTypCd = crtfTypCd; }
        public void setCrtfNo(String crtfNo) { this.crtfNo = crtfNo; }
        public void setCrtfMatrDt(String crtfMatrDt) { this.crtfMatrDt = crtfMatrDt; }
        public void setAddr(String addr) { this.addr = addr; }
        public void setHousDrgstAddr(String housDrgstAddr) { this.housDrgstAddr = housDrgstAddr; }
        public void setGenderCd(String genderCd) { this.genderCd = genderCd; }
        public void setMarrgSituationCd(String marrgSituationCd) { this.marrgSituationCd = marrgSituationCd; }
        public void setBirthDt(String birthDt) { this.birthDt = birthDt; }
        public void setCareerTypCd(String careerTypCd) { this.careerTypCd = careerTypCd; }
        public void setStateRgnCd(String stateRgnCd) { this.stateRgnCd = stateRgnCd; }
        public void setDomOversFLgCd(String domOversFLgCd) { this.domOversFLgCd = domOversFLgCd; }
        public void setIdcardTypCd(String idcardTypCd) { this.idcardTypCd = idcardTypCd; }
        public void setEmplyFlg(String emplyFlg) { this.emplyFlg = emplyFlg; }
        public void setSrhdFlg(String srhdFlg) { this.srhdFlg = srhdFlg; }
        public void setSpsName(String spsName) { this.spsName = spsName; }
        public void setSpsEngName(String spsEngName) { this.spsEngName = spsEngName; }
        public void setSpsCrtfTypCd(String spsCrtfTypCd) { this.spsCrtfTypCd = spsCrtfTypCd; }
        public void setSpsCrtfNo(String spsCrtfNo) { this.spsCrtfNo = spsCrtfNo; }
        public void setSpsTelNo(String spsTelNo) { this.spsTelNo = spsTelNo; }
        public void setWorkunitNm(String workunitNm) { this.workunitNm = workunitNm; }
        public void setWorkunitAddr(String workunitAddr) { this.workunitAddr = workunitAddr; }
        public void setAdminCmprmntCd(String adminCmprmntCd) { this.adminCmprmntCd = adminCmprmntCd; }
    }

    /**
     * Partially updates personal customer basic and/or extended information.
     *
     * @param request update request with at least {@code custNo} populated
     * @return {@link UpdateResponse} with result code and message
     */
    public UpdateResponse updateCustomer(UpdateCustomerRequest request) {
        UpdateResponse response = new UpdateResponse();
        response.respCode = "E99999";
        response.respMsg = "PROCESSING ERROR";

        // 1) Validate customer number
        if (request.custNo == null || request.custNo.trim().isEmpty()) {
            response.respCode = "F20003";
            response.respMsg = "Customer number is required";
            return response;
        }

        try {
            // 2) Verify the customer exists and is a personal customer (type '0')
            String custTypCd = findCustomerType(request.custNo);
            if (custTypCd == null) {
                response.respCode = "F20000";
                response.respMsg = "Customer not found";
                return response;
            }
            if (!"0".equals(custTypCd)) {
                response.respCode = "F20002";
                response.respMsg = "Customer type is not personal";
                return response;
            }

            // 3) Begin transaction
            connection.setAutoCommit(false);

            boolean basicUpdated = false;
            boolean personalUpdated = false;

            // 4) Update customer basic information if any basic fields are provided
            if (hasBasicFields(request)) {
                updateBasicInfo(request);
                basicUpdated = true;
            }

            // 5) Update personal customer information if any personal fields are provided
            if (hasPersonalFields(request)) {
                updatePersonalInfo(request);
                personalUpdated = true;
            }

            // 6) Commit transaction
            connection.commit();

            response.respCode = "000000";
            if (basicUpdated && personalUpdated) {
                response.respMsg = "Both info updated successfully";
            } else if (basicUpdated) {
                response.respMsg = "Basic info updated successfully";
            } else if (personalUpdated) {
                response.respMsg = "Personal info updated successfully";
            } else {
                response.respMsg = "No changes detected";
            }

        } catch (SQLException e) {
            rollback();
            response.respCode = "E12004";
            response.respMsg = "Failed to update basic info";
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
                return rs.next() ? rs.getString(1) : null;
            }
        }
    }

    private boolean hasBasicFields(UpdateCustomerRequest r) {
        return isPresent(r.custNm) || isPresent(r.custEngNm) || isPresent(r.custLvlCd)
                || isPresent(r.mobileNo) || isPresent(r.eMail) || isPresent(r.crtfTypCd)
                || isPresent(r.crtfNo) || isPresent(r.crtfMatrDt);
    }

    private boolean hasPersonalFields(UpdateCustomerRequest r) {
        return isPresent(r.addr) || isPresent(r.housDrgstAddr) || isPresent(r.genderCd)
                || isPresent(r.marrgSituationCd) || isPresent(r.birthDt)
                || isPresent(r.careerTypCd) || isPresent(r.stateRgnCd)
                || isPresent(r.domOversFLgCd) || isPresent(r.idcardTypCd)
                || isPresent(r.emplyFlg) || isPresent(r.srhdFlg) || isPresent(r.spsName)
                || isPresent(r.spsEngName) || isPresent(r.spsCrtfTypCd)
                || isPresent(r.spsCrtfNo) || isPresent(r.spsTelNo)
                || isPresent(r.workunitNm) || isPresent(r.workunitAddr)
                || isPresent(r.adminCmprmntCd);
    }

    private boolean isPresent(String value) {
        return value != null && !value.trim().isEmpty();
    }

    private void updateBasicInfo(UpdateCustomerRequest r) throws SQLException {
        String sql = "UPDATE CUSTOMER_BASIC_INFO SET "
                + "CUST_NM = CASE WHEN ? IS NOT NULL THEN ? ELSE CUST_NM END, "
                + "CUST_ENG_NM = CASE WHEN ? IS NOT NULL THEN ? ELSE CUST_ENG_NM END, "
                + "CUST_LVL_CD = CASE WHEN ? IS NOT NULL THEN ? ELSE CUST_LVL_CD END, "
                + "MOBILE_NO = CASE WHEN ? IS NOT NULL THEN ? ELSE MOBILE_NO END, "
                + "E_MAIL = CASE WHEN ? IS NOT NULL THEN ? ELSE E_MAIL END, "
                + "CRTF_TYP_CD = CASE WHEN ? IS NOT NULL THEN ? ELSE CRTF_TYP_CD END, "
                + "CRTF_NO = CASE WHEN ? IS NOT NULL THEN ? ELSE CRTF_NO END, "
                + "CRTF_MATR_DT = CASE WHEN ? IS NOT NULL THEN ? ELSE CRTF_MATR_DT END, "
                + "LAST_UPD_DT = CURRENT_DATE, LAST_UPD_TM = CURRENT_TIME "
                + "WHERE CUST_NO = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, emptyToNull(r.custNm));
            ps.setString(2, emptyToNull(r.custNm));
            ps.setString(3, emptyToNull(r.custEngNm));
            ps.setString(4, emptyToNull(r.custEngNm));
            ps.setString(5, emptyToNull(r.custLvlCd));
            ps.setString(6, emptyToNull(r.custLvlCd));
            ps.setString(7, emptyToNull(r.mobileNo));
            ps.setString(8, emptyToNull(r.mobileNo));
            ps.setString(9, emptyToNull(r.eMail));
            ps.setString(10, emptyToNull(r.eMail));
            ps.setString(11, emptyToNull(r.crtfTypCd));
            ps.setString(12, emptyToNull(r.crtfTypCd));
            ps.setString(13, emptyToNull(r.crtfNo));
            ps.setString(14, emptyToNull(r.crtfNo));
            ps.setString(15, emptyToNull(r.crtfMatrDt));
            ps.setString(16, emptyToNull(r.crtfMatrDt));
            ps.setString(17, r.custNo);
            ps.executeUpdate();
        }
    }

    private void updatePersonalInfo(UpdateCustomerRequest r) throws SQLException {
        String sql = "UPDATE PERSONAL_CUSTOMER_INFO SET "
                + "ADDR = CASE WHEN ? IS NOT NULL THEN ? ELSE ADDR END, "
                + "HOUSDRGST_ADDR = CASE WHEN ? IS NOT NULL THEN ? ELSE HOUSDRGST_ADDR END, "
                + "GENDER_CD = CASE WHEN ? IS NOT NULL THEN ? ELSE GENDER_CD END, "
                + "MARRG_SITUATION_CD = CASE WHEN ? IS NOT NULL THEN ? ELSE MARRG_SITUATION_CD END, "
                + "BIRTH_DT = CASE WHEN ? IS NOT NULL THEN ? ELSE BIRTH_DT END, "
                + "CAREER_TYP_CD = CASE WHEN ? IS NOT NULL THEN ? ELSE CAREER_TYP_CD END, "
                + "STATE_AND_RGN_CD = CASE WHEN ? IS NOT NULL THEN ? ELSE STATE_AND_RGN_CD END, "
                + "DOM_OVERS_FLG_CD = CASE WHEN ? IS NOT NULL THEN ? ELSE DOM_OVERS_FLG_CD END, "
                + "IDCARD_TYP_CD = CASE WHEN ? IS NOT NULL THEN ? ELSE IDCARD_TYP_CD END, "
                + "EMPLY_FLG = CASE WHEN ? IS NOT NULL THEN ? ELSE EMPLY_FLG END, "
                + "SHRHD_FLG = CASE WHEN ? IS NOT NULL THEN ? ELSE SHRHD_FLG END, "
                + "SPS_NAME = CASE WHEN ? IS NOT NULL THEN ? ELSE SPS_NAME END, "
                + "SPS_ENG_NAME = CASE WHEN ? IS NOT NULL THEN ? ELSE SPS_ENG_NAME END, "
                + "SPS_CRTF_TYP_CD = CASE WHEN ? IS NOT NULL THEN ? ELSE SPS_CRTF_TYP_CD END, "
                + "SPS_CRTF_NO = CASE WHEN ? IS NOT NULL THEN ? ELSE SPS_CRTF_NO END, "
                + "SPS_TEL_NO = CASE WHEN ? IS NOT NULL THEN ? ELSE SPS_TEL_NO END, "
                + "WORK_UNIT_NM = CASE WHEN ? IS NOT NULL THEN ? ELSE WORK_UNIT_NM END, "
                + "WORK_UNIT_ADDR = CASE WHEN ? IS NOT NULL THEN ? ELSE WORK_UNIT_ADDR END, "
                + "ADMIN_CMPRMNT_CD = CASE WHEN ? IS NOT NULL THEN ? ELSE ADMIN_CMPRMNT_CD END, "
                + "LAST_UPD_DT = CURRENT_DATE, LAST_UPD_TM = CURRENT_TIME "
                + "WHERE CUST_NO = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            int i = 1;
            i = setNullablePair(ps, i, r.addr);
            i = setNullablePair(ps, i, r.housDrgstAddr);
            i = setNullablePair(ps, i, r.genderCd);
            i = setNullablePair(ps, i, r.marrgSituationCd);
            i = setNullablePair(ps, i, r.birthDt);
            i = setNullablePair(ps, i, r.careerTypCd);
            i = setNullablePair(ps, i, r.stateRgnCd);
            i = setNullablePair(ps, i, r.domOversFLgCd);
            i = setNullablePair(ps, i, r.idcardTypCd);
            i = setNullablePair(ps, i, r.emplyFlg);
            i = setNullablePair(ps, i, r.srhdFlg);
            i = setNullablePair(ps, i, r.spsName);
            i = setNullablePair(ps, i, r.spsEngName);
            i = setNullablePair(ps, i, r.spsCrtfTypCd);
            i = setNullablePair(ps, i, r.spsCrtfNo);
            i = setNullablePair(ps, i, r.spsTelNo);
            i = setNullablePair(ps, i, r.workunitNm);
            i = setNullablePair(ps, i, r.workunitAddr);
            i = setNullablePair(ps, i, r.adminCmprmntCd);
            ps.setString(i, r.custNo);
            ps.executeUpdate();
        }
    }

    /** Sets a CASE-WHEN pair: first param is the null-check value, second is the update value. */
    private int setNullablePair(PreparedStatement ps, int startIndex, String value)
            throws SQLException {
        String v = emptyToNull(value);
        ps.setString(startIndex, v);
        ps.setString(startIndex + 1, v);
        return startIndex + 2;
    }

    private String emptyToNull(String value) {
        return (value == null || value.trim().isEmpty()) ? null : value;
    }

    private void rollback() {
        try {
            connection.rollback();
        } catch (SQLException ignored) {
        }
    }
}

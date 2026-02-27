import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

/**
 * Create Personal Customer Service – simple version (个人客户开立服务（简版）).
 *
 * <p>Implements personal customer creation with deduplication and automatic
 * extraction of date-of-birth and gender from the national ID card number.
 *
 * <p>Converted from COBOL program CRTPERC01 (3.CreatePerCustInfo.cbl).
 */
public class CreatePersonalCustomerService {

    private static final String DEFAULT_TENANT_NO = "001";
    private static final String ID_CARD_TYPE = "01";

    private final Connection connection;

    public CreatePersonalCustomerService(Connection connection) {
        this.connection = connection;
    }

    /**
     * Response object returned by {@link #createCustomer}.
     */
    public static class CreateCustomerResponse {
        private String respCode;
        private String respMsg;
        private String custNo;
        private String tenantNo;

        public String getRespCode() { return respCode; }
        public String getRespMsg() { return respMsg; }
        public String getCustNo() { return custNo; }
        public String getTenantNo() { return tenantNo; }
    }

    /**
     * Creates a new personal customer or returns an existing one when the credential
     * already exists and the customer name matches.
     *
     * @param crtfNo     credential number (证件号码)
     * @param crtfTypCd  credential type code (证件类型代码)
     * @param custNm     customer name (客户名称)
     * @param operTelrNo operator teller number (操作柜员号)
     * @return {@link CreateCustomerResponse} containing result code, message, generated
     *         customer number and tenant number
     */
    public CreateCustomerResponse createCustomer(String crtfNo, String crtfTypCd,
            String custNm, String operTelrNo) {
        CreateCustomerResponse response = new CreateCustomerResponse();
        response.respCode = "E99999";
        response.respMsg = "PROCESSING ERROR";

        // 1) Validate required parameters
        if (crtfNo == null || crtfNo.trim().isEmpty()) {
            response.respCode = "F20005";
            response.respMsg = "证件号码不能为空";
            return response;
        }
        if (crtfTypCd == null || crtfTypCd.trim().isEmpty()) {
            response.respCode = "F20004";
            response.respMsg = "证件类型代码不能为空";
            return response;
        }
        if (custNm == null || custNm.trim().isEmpty()) {
            response.respCode = "F20007";
            response.respMsg = "客户名称不能为空";
            return response;
        }

        try {
            // 2) Check whether the customer already exists
            ExistingCustomer existing = findExistingCustomer(crtfTypCd, crtfNo);
            if (existing != null) {
                if (existing.custNm.trim().equals(custNm.trim())) {
                    response.respCode = "000000";
                    response.respMsg = "客户已存在，返回现有客户信息";
                    response.custNo = existing.custNo;
                    response.tenantNo = existing.tenantNo;
                } else {
                    response.respCode = "F20008";
                    response.respMsg = "证件号已存在但客户名称不匹配";
                }
                return response;
            }

            // 3) Begin transaction
            connection.setAutoCommit(false);

            // 4) Generate a new customer number
            String newCustNo = generateCustNo();

            // 5) Extract ID-card info if the credential type is a national ID card
            String genderCd = null;
            String birthDt = null;
            if (ID_CARD_TYPE.equals(crtfTypCd)) {
                IdCardInfo idInfo = extractIdCardInfo(crtfNo);
                genderCd = idInfo.genderCd;
                birthDt = idInfo.birthDt;
            }

            // 6) Insert customer basic information
            insertCustomerBasicInfo(newCustNo, crtfTypCd, crtfNo, custNm, operTelrNo);

            // 7) Insert personal customer information
            insertPersonalCustomerInfo(newCustNo, genderCd, birthDt, operTelrNo);

            // 8) Commit transaction
            connection.commit();

            response.respCode = "000000";
            response.respMsg = "客户开立成功";
            response.custNo = newCustNo;
            response.tenantNo = DEFAULT_TENANT_NO;

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

    private static class ExistingCustomer {
        String custNo;
        String tenantNo;
        String custNm;
    }

    private ExistingCustomer findExistingCustomer(String crtfTypCd, String crtfNo)
            throws SQLException {
        String sql = "SELECT CUST_NO, TENANT_NO, CUST_NM FROM CUSTOMER_BASIC_INFO "
                + "WHERE CRTF_TYP_CD = ? AND CRTF_NO = ? AND VALID_FLG = '1'";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, crtfTypCd);
            ps.setString(2, crtfNo);
            try (ResultSet rs = ps.executeQuery()) {
                if (rs.next()) {
                    ExistingCustomer ec = new ExistingCustomer();
                    ec.custNo = rs.getString("CUST_NO");
                    ec.tenantNo = rs.getString("TENANT_NO");
                    ec.custNm = rs.getString("CUST_NM");
                    return ec;
                }
            }
        }
        return null;
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
        // Fallback: timestamp-based ID
        String ts = LocalDate.now().format(DateTimeFormatter.ofPattern("yyMMdd"))
                + System.currentTimeMillis() % 1_000_000L;
        return "CUST" + ts.substring(0, 6);
    }

    private void insertCustomerBasicInfo(String custNo, String crtfTypCd, String crtfNo,
            String custNm, String operTelrNo) throws SQLException {
        String sql = "INSERT INTO CUSTOMER_BASIC_INFO "
                + "(TENANT_NO, CUST_NO, CUST_TYP_CD, CUST_LVL_CD, "
                + " CRTF_TYP_CD, CRTF_NO, CUST_NM, VALID_FLG, "
                + " CRT_TELR_NO, UPD_TELR_NO, CRT_TM, UPD_TM) "
                + "VALUES (?, ?, '0', '1', ?, ?, ?, '1', ?, ?, "
                + "CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, DEFAULT_TENANT_NO);
            ps.setString(2, custNo);
            ps.setString(3, crtfTypCd);
            ps.setString(4, crtfNo);
            ps.setString(5, custNm);
            ps.setString(6, operTelrNo);
            ps.setString(7, operTelrNo);
            ps.executeUpdate();
        }
    }

    private void insertPersonalCustomerInfo(String custNo, String genderCd,
            String birthDt, String operTelrNo) throws SQLException {
        String sql = "INSERT INTO PERSONAL_CUSTOMER_INFO "
                + "(TENANT_NO, CUST_NO, GENDER_CD, BIRTH_DT, VALID_FLG, "
                + " CRT_TELR_NO, UPD_TELR_NO, CRT_TM, UPD_TM) "
                + "VALUES (?, ?, ?, ?, '1', ?, ?, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, DEFAULT_TENANT_NO);
            ps.setString(2, custNo);
            ps.setString(3, genderCd);
            ps.setString(4, birthDt);
            ps.setString(5, operTelrNo);
            ps.setString(6, operTelrNo);
            ps.executeUpdate();
        }
    }

    // -------------------------------------------------------------------------
    // ID card information extraction
    // -------------------------------------------------------------------------

    private static class IdCardInfo {
        String genderCd;
        String birthDt;
    }

    /**
     * Extracts date-of-birth and gender from a Chinese national ID card number.
     * Supports both 15-digit (old) and 18-digit (new) formats.
     */
    private IdCardInfo extractIdCardInfo(String idNo) {
        IdCardInfo info = new IdCardInfo();
        if (idNo == null) {
            return info;
        }
        String trimmed = idNo.trim();
        if (trimmed.length() == 18) {
            info.birthDt = trimmed.substring(6, 14);
            int genderDigit = Integer.parseInt(trimmed.substring(16, 17));
            info.genderCd = (genderDigit % 2 == 0) ? "2" : "1";
        } else if (trimmed.length() == 15) {
            info.birthDt = "19" + trimmed.substring(6, 12);
            int genderDigit = Integer.parseInt(trimmed.substring(14, 15));
            info.genderCd = (genderDigit % 2 == 0) ? "2" : "1";
        }
        return info;
    }

    private void rollback() {
        try {
            connection.rollback();
        } catch (SQLException ignored) {
        }
    }
}

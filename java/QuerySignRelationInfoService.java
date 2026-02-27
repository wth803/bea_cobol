import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

/**
 * Query Sign Relation Info Service (客户签约关系查询服务).
 *
 * <p>Implements customer signing relationship information query by customer number.
 * The search can be performed by credential number, account number, customer name,
 * or signing sub-type code.
 *
 * <p>Converted from COBOL program QURYSIGNRELATIONINFO (16.QurySignRelationInfo.cbl).
 */
public class QuerySignRelationInfoService {

    private final Connection connection;

    public QuerySignRelationInfoService(Connection connection) {
        this.connection = connection;
    }

    /**
     * A single signing relationship record.
     */
    public static class SignRelationInfo {
        private String signNo;
        private String signType;
        private String signStatus;
        private String signDate;
        private BigDecimal signAmount;
        private String signDesc;

        public String getSignNo() { return signNo; }
        public String getSignType() { return signType; }
        public String getSignStatus() { return signStatus; }
        public String getSignDate() { return signDate; }
        public BigDecimal getSignAmount() { return signAmount; }
        public String getSignDesc() { return signDesc; }
    }

    /**
     * Response object returned by {@link #query}.
     */
    public static class QuerySignRelationResponse {
        private int returnCode;
        private String returnMessage;
        private List<SignRelationInfo> records = new ArrayList<>();

        public int getReturnCode() { return returnCode; }
        public String getReturnMessage() { return returnMessage; }
        public List<SignRelationInfo> getRecords() { return records; }
    }

    /**
     * Queries customer signing relationship records by various optional criteria.
     * At least one of {@code crtfNo} or {@code custAcctNo} must be non-blank.
     *
     * @param crtfNo          credential number (证件号码), optional
     * @param crtfTypCd       credential type code (证件类型代码), optional
     * @param custAcctNo      customer account number (客户账号), optional
     * @param custNm          customer name (客户名称), optional
     * @param signSmlTypTypCd signing sub-type code (签约小类型代码), optional
     * @return {@link QuerySignRelationResponse} containing result code, message, and
     *         matching signing relationship records
     */
    public QuerySignRelationResponse query(String crtfNo, String crtfTypCd,
            String custAcctNo, String custNm, String signSmlTypTypCd) {

        QuerySignRelationResponse response = new QuerySignRelationResponse();
        response.returnCode = 0;

        // 1) Validate required parameters
        boolean noCrtfNo = crtfNo == null || crtfNo.trim().isEmpty();
        boolean noCustAcctNo = custAcctNo == null || custAcctNo.trim().isEmpty();
        if (noCrtfNo && noCustAcctNo) {
            response.returnCode = 1001;
            response.returnMessage = "证件号码和客户账号不能同时为空";
            return response;
        }

        // 2) Validate credential type code if provided
        if (crtfTypCd != null && !crtfTypCd.trim().isEmpty()) {
            if (!crtfTypCd.equals("01") && !crtfTypCd.equals("02")
                    && !crtfTypCd.equals("03")) {
                response.returnCode = 1002;
                response.returnMessage = "证件类型代码不正确";
                return response;
            }
        }

        try {
            // 3) Query signing relationship records
            List<SignRelationInfo> records = queryRecords(crtfNo, crtfTypCd, custAcctNo,
                    custNm, signSmlTypTypCd);

            if (records.isEmpty()) {
                response.returnCode = 1003;
                response.returnMessage = "未找到匹配的签约关系";
            } else {
                response.returnCode = 0;
                response.returnMessage = "查询成功";
                response.records = records;
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

    private List<SignRelationInfo> queryRecords(String crtfNo, String crtfTypCd,
            String custAcctNo, String custNm, String signSmlTypTypCd) throws SQLException {

        StringBuilder sql = new StringBuilder(
                "SELECT SIGN_NO, SIGN_TYPE, SIGN_STATUS, SIGN_DATE, SIGN_AMOUNT, SIGN_DESC "
                + "FROM CUSTOMER_SIGN_RELATION WHERE 1=1");

        if (crtfNo != null && !crtfNo.trim().isEmpty()) {
            sql.append(" AND CRTF_NO = ?");
        }
        if (crtfTypCd != null && !crtfTypCd.trim().isEmpty()) {
            sql.append(" AND CRTF_TYP_CD = ?");
        }
        if (custAcctNo != null && !custAcctNo.trim().isEmpty()) {
            sql.append(" AND CUST_ACCT_NO = ?");
        }
        if (custNm != null && !custNm.trim().isEmpty()) {
            sql.append(" AND CUST_NM = ?");
        }
        if (signSmlTypTypCd != null && !signSmlTypTypCd.trim().isEmpty()) {
            sql.append(" AND SIGN_SMLTYP_TYP_CD = ?");
        }

        try (PreparedStatement ps = connection.prepareStatement(sql.toString())) {
            int idx = 1;
            if (crtfNo != null && !crtfNo.trim().isEmpty()) {
                ps.setString(idx++, crtfNo);
            }
            if (crtfTypCd != null && !crtfTypCd.trim().isEmpty()) {
                ps.setString(idx++, crtfTypCd);
            }
            if (custAcctNo != null && !custAcctNo.trim().isEmpty()) {
                ps.setString(idx++, custAcctNo);
            }
            if (custNm != null && !custNm.trim().isEmpty()) {
                ps.setString(idx++, custNm);
            }
            if (signSmlTypTypCd != null && !signSmlTypTypCd.trim().isEmpty()) {
                ps.setString(idx, signSmlTypTypCd);
            }

            List<SignRelationInfo> records = new ArrayList<>();
            try (ResultSet rs = ps.executeQuery()) {
                while (rs.next()) {
                    SignRelationInfo info = new SignRelationInfo();
                    info.signNo = rs.getString("SIGN_NO");
                    info.signType = rs.getString("SIGN_TYPE");
                    info.signStatus = rs.getString("SIGN_STATUS");
                    info.signDate = rs.getString("SIGN_DATE");
                    info.signAmount = rs.getBigDecimal("SIGN_AMOUNT");
                    info.signDesc = rs.getString("SIGN_DESC");
                    records.add(info);
                }
            }
            return records;
        }
    }
}

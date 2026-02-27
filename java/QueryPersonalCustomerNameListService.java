import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

/**
 * Query Personal Customer Name List Service (对私客户名单信息查询服务).
 *
 * <p>Implements personal customer watchlist and name-list information query by
 * customer number.
 *
 * <p>Converted from COBOL program QURYPERCUSTNAMELIST
 * (14.QuryPerCustNameListInfoByCustNo.cbl).
 */
public class QueryPersonalCustomerNameListService {

    private final Connection connection;

    public QueryPersonalCustomerNameListService(Connection connection) {
        this.connection = connection;
    }

    /**
     * A single name-list entry for a personal customer.
     */
    public static class NameListInfo {
        private String custNo;
        private String crtfTypCd;
        private String crtfNo;
        private String nmSnglTypCd;
        private String dataSorcCd;
        private String orgDismnCd;
        private String ctrlFlg;
        private String chkFlgCd;
        private String efftDt;
        private String efftTm;
        private String invalidDt;
        private String invalidTm;
        private String validFlg;

        public String getCustNo() { return custNo; }
        public String getCrtfTypCd() { return crtfTypCd; }
        public String getCrtfNo() { return crtfNo; }
        public String getNmSnglTypCd() { return nmSnglTypCd; }
        public String getDataSorcCd() { return dataSorcCd; }
        public String getOrgDismnCd() { return orgDismnCd; }
        public String getCtrlFlg() { return ctrlFlg; }
        public String getChkFlgCd() { return chkFlgCd; }
        public String getEfftDt() { return efftDt; }
        public String getEfftTm() { return efftTm; }
        public String getInvalidDt() { return invalidDt; }
        public String getInvalidTm() { return invalidTm; }
        public String getValidFlg() { return validFlg; }
    }

    /**
     * Response object returned by {@link #query}.
     */
    public static class QueryNameListResponse {
        private int returnCode;
        private String returnMessage;
        private List<NameListInfo> records = new ArrayList<>();

        public int getReturnCode() { return returnCode; }
        public String getReturnMessage() { return returnMessage; }
        public List<NameListInfo> getRecords() { return records; }
    }

    /**
     * Queries name-list information for the given personal customer.
     *
     * @param custNo customer number (客户编号)
     * @return {@link QueryNameListResponse} with result code, message, and name-list
     *         records
     */
    public QueryNameListResponse query(String custNo) {
        QueryNameListResponse response = new QueryNameListResponse();
        response.returnCode = 0;

        // 1) Validate required parameter
        if (custNo == null || custNo.trim().isEmpty()) {
            response.returnCode = 1001;
            response.returnMessage = "客户编号不能为空";
            return response;
        }

        try {
            // 2) Query name-list records
            List<NameListInfo> records = queryRecords(custNo);

            if (records.isEmpty()) {
                response.returnCode = 20003;
                response.returnMessage = "未找到客户名单信息";
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

    private List<NameListInfo> queryRecords(String custNo) throws SQLException {
        String sql = "SELECT CUST_NO, CRTF_TYP_CD, CRTF_NO, NM_SNGL_TYP_CD, "
                + "DATA_SORC_CD, ORG_DISMN_CD, CTRL_FLG, CHK_FLG_CD, "
                + "EFFT_DT, EFFT_TM, INVALID_DT, INVALID_TM, VALID_FLG "
                + "FROM PERSONAL_CUSTOMER_NAME_LIST WHERE CUST_NO = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, custNo);

            List<NameListInfo> records = new ArrayList<>();
            try (ResultSet rs = ps.executeQuery()) {
                while (rs.next()) {
                    NameListInfo info = new NameListInfo();
                    info.custNo = rs.getString("CUST_NO");
                    info.crtfTypCd = rs.getString("CRTF_TYP_CD");
                    info.crtfNo = rs.getString("CRTF_NO");
                    info.nmSnglTypCd = rs.getString("NM_SNGL_TYP_CD");
                    info.dataSorcCd = rs.getString("DATA_SORC_CD");
                    info.orgDismnCd = rs.getString("ORG_DISMN_CD");
                    info.ctrlFlg = rs.getString("CTRL_FLG");
                    info.chkFlgCd = rs.getString("CHK_FLG_CD");
                    info.efftDt = rs.getString("EFFT_DT");
                    info.efftTm = rs.getString("EFFT_TM");
                    info.invalidDt = rs.getString("INVALID_DT");
                    info.invalidTm = rs.getString("INVALID_TM");
                    info.validFlg = rs.getString("VALID_FLG");
                    records.add(info);
                }
            }
            return records;
        }
    }
}

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

/**
 * Query Customer Account By Account Service (按账号查询客户账户路由服务).
 *
 * <p>Implements customer account routing information query by route value and
 * route type.
 *
 * <p>Converted from COBOL program QRYCUSTACCTINFO (7.QuryCustAcctInfoByCustAcct.cbl).
 */
public class QueryCustomerAccountByAcctService {

    private final Connection connection;

    public QueryCustomerAccountByAcctService(Connection connection) {
        this.connection = connection;
    }

    /**
     * A single customer account routing record returned in the query result.
     */
    public static class CustAcctRouteInfo {
        private long id;
        private String tenantNo;
        private String custNo;
        private String afsProDtNo;
        private String baseProDtNo;
        private String mainAcctNo;
        private String operTypCd;
        private String relaSeqNo;
        private String routeTypCd;
        private String routeVal;
        private String validFlg;
        private String crtTelrNo;
        private String updTelrNo;
        private String updTm;
        private String crtTm;

        public long getId() { return id; }
        public String getTenantNo() { return tenantNo; }
        public String getCustNo() { return custNo; }
        public String getAfsProDtNo() { return afsProDtNo; }
        public String getBaseProDtNo() { return baseProDtNo; }
        public String getMainAcctNo() { return mainAcctNo; }
        public String getOperTypCd() { return operTypCd; }
        public String getRelaSeqNo() { return relaSeqNo; }
        public String getRouteTypCd() { return routeTypCd; }
        public String getRouteVal() { return routeVal; }
        public String getValidFlg() { return validFlg; }
        public String getCrtTelrNo() { return crtTelrNo; }
        public String getUpdTelrNo() { return updTelrNo; }
        public String getUpdTm() { return updTm; }
        public String getCrtTm() { return crtTm; }
    }

    /**
     * Response object returned by {@link #query}.
     */
    public static class QueryByAcctResponse {
        private int returnCode;
        private String returnMessage;
        private List<CustAcctRouteInfo> records = new ArrayList<>();

        public int getReturnCode() { return returnCode; }
        public String getReturnMessage() { return returnMessage; }
        public List<CustAcctRouteInfo> getRecords() { return records; }
    }

    /**
     * Queries customer account routing information by route value and route type.
     *
     * @param tenantNo   tenant number (租户号)
     * @param routeVal   route value (路由值)
     * @param routeTypCd route type code (路由类型)
     * @param relaSeqNo  optional relation sequence number; pass {@code null} or empty
     *                   to omit this filter (关联序号，可选)
     * @return {@link QueryByAcctResponse} containing result code, message, and
     *         a list of matching routing records
     */
    public QueryByAcctResponse query(String tenantNo, String routeVal, String routeTypCd,
            String relaSeqNo) {
        QueryByAcctResponse response = new QueryByAcctResponse();
        response.returnCode = 0;

        // 1) Validate required parameters
        if (tenantNo == null || tenantNo.trim().isEmpty()) {
            response.returnCode = 1001;
            response.returnMessage = "租户号不能为空";
            return response;
        }
        if (routeVal == null || routeVal.trim().isEmpty()) {
            response.returnCode = 1002;
            response.returnMessage = "路由值不能为空";
            return response;
        }
        if (routeTypCd == null || routeTypCd.trim().isEmpty()) {
            response.returnCode = 1003;
            response.returnMessage = "路由类型不能为空";
            return response;
        }

        try {
            // 2) Query routing records
            List<CustAcctRouteInfo> records = queryRecords(tenantNo, routeVal, routeTypCd,
                    relaSeqNo);

            response.returnCode = 0;
            response.returnMessage = "查询成功";
            response.records = records;

        } catch (SQLException e) {
            response.returnCode = 9999;
            response.returnMessage = "数据库查询失败";
        }

        return response;
    }

    // -------------------------------------------------------------------------
    // Private helpers
    // -------------------------------------------------------------------------

    private List<CustAcctRouteInfo> queryRecords(String tenantNo, String routeVal,
            String routeTypCd, String relaSeqNo) throws SQLException {
        StringBuilder sql = new StringBuilder(
                "SELECT ID, TENANT_NO, CUST_NO, AFS_PRODT_NO, BASE_PRODT_NO, "
                + "MAIN_ACCT_NO, OPER_TYP_CD, RELA_SEQ_NO, ROUTE_TYP_CD, ROUTE_VAL, "
                + "VALID_FLG, CRT_TELR_NO, UPD_TELR_NO, UPD_TM, CRT_TM "
                + "FROM THSBCECIF_CUST_ACCT_INFO "
                + "WHERE TENANT_NO = ? AND ROUTE_VAL = ? AND ROUTE_TYP_CD = ? "
                + "AND VALID_FLG = '1'");

        boolean hasRelaSeqNo = relaSeqNo != null && !relaSeqNo.trim().isEmpty();
        if (hasRelaSeqNo) {
            sql.append(" AND RELA_SEQ_NO = ?");
        }

        try (PreparedStatement ps = connection.prepareStatement(sql.toString())) {
            ps.setString(1, tenantNo);
            ps.setString(2, routeVal);
            ps.setString(3, routeTypCd);
            if (hasRelaSeqNo) {
                ps.setString(4, relaSeqNo);
            }

            List<CustAcctRouteInfo> records = new ArrayList<>();
            try (ResultSet rs = ps.executeQuery()) {
                while (rs.next()) {
                    CustAcctRouteInfo info = new CustAcctRouteInfo();
                    info.id = rs.getLong("ID");
                    info.tenantNo = rs.getString("TENANT_NO");
                    info.custNo = rs.getString("CUST_NO");
                    info.afsProDtNo = rs.getString("AFS_PRODT_NO");
                    info.baseProDtNo = rs.getString("BASE_PRODT_NO");
                    info.mainAcctNo = rs.getString("MAIN_ACCT_NO");
                    info.operTypCd = rs.getString("OPER_TYP_CD");
                    info.relaSeqNo = rs.getString("RELA_SEQ_NO");
                    info.routeTypCd = rs.getString("ROUTE_TYP_CD");
                    info.routeVal = rs.getString("ROUTE_VAL");
                    info.validFlg = rs.getString("VALID_FLG");
                    info.crtTelrNo = rs.getString("CRT_TELR_NO");
                    info.updTelrNo = rs.getString("UPD_TELR_NO");
                    info.updTm = rs.getString("UPD_TM");
                    info.crtTm = rs.getString("CRT_TM");
                    records.add(info);
                }
            }
            return records;
        }
    }
}

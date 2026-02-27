import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

/**
 * Query Customer Account By Customer Number Service (按客户号查询账户路由服务).
 *
 * <p>Implements customer account routing information query by customer number
 * and route type.
 *
 * <p>Converted from COBOL program QURYCUSTACCTINFOBYCUSTNO
 * (8.QuryCustAcctInfoByCustNo.cbl).
 */
public class QueryCustomerAccountByCustNoService {

    private final Connection connection;

    public QueryCustomerAccountByCustNoService(Connection connection) {
        this.connection = connection;
    }

    /**
     * A single customer account routing record returned in the query result.
     */
    public static class CustAcctRouteInfo {
        private String custNo;
        private String afsProDtNo;
        private String baseProDtNo;
        private String mainAcctNo;
        private String operTypCd;
        private String relaSeqNo;
        private String routeTypCd;
        private String routeVal;
        private String validFlg;

        public String getCustNo() { return custNo; }
        public String getAfsProDtNo() { return afsProDtNo; }
        public String getBaseProDtNo() { return baseProDtNo; }
        public String getMainAcctNo() { return mainAcctNo; }
        public String getOperTypCd() { return operTypCd; }
        public String getRelaSeqNo() { return relaSeqNo; }
        public String getRouteTypCd() { return routeTypCd; }
        public String getRouteVal() { return routeVal; }
        public String getValidFlg() { return validFlg; }
    }

    /**
     * Response object returned by {@link #query}.
     */
    public static class QueryByCustNoResponse {
        private int returnCode;
        private String returnMessage;
        private List<CustAcctRouteInfo> records = new ArrayList<>();

        public int getReturnCode() { return returnCode; }
        public String getReturnMessage() { return returnMessage; }
        public List<CustAcctRouteInfo> getRecords() { return records; }
    }

    /**
     * Queries customer account routing records by customer number and optional filters.
     *
     * @param tenantNo   tenant number (租户号)
     * @param custNo     customer number (客户号)
     * @param routeTypCd route type code used to filter the results (路由值类型，必填)
     * @param statusCd   optional status/valid-flag filter; pass {@code null} or empty to
     *                   return all statuses (状态代码，可选)
     * @return {@link QueryByCustNoResponse} containing result code, message, and matching
     *         records
     */
    public QueryByCustNoResponse query(String tenantNo, String custNo, String routeTypCd,
            String statusCd) {
        QueryByCustNoResponse response = new QueryByCustNoResponse();
        response.returnCode = 0;

        // 1) Validate required parameters
        if (tenantNo == null || tenantNo.trim().isEmpty()) {
            response.returnCode = 1001;
            response.returnMessage = "租户号不能为空";
            return response;
        }
        if (custNo == null || custNo.trim().isEmpty()) {
            response.returnCode = 1002;
            response.returnMessage = "客户号不能为空";
            return response;
        }
        if (routeTypCd == null || routeTypCd.trim().isEmpty()) {
            response.returnCode = 1003;
            response.returnMessage = "路由值类型不能为空";
            return response;
        }

        try {
            // 2) Query routing records
            List<CustAcctRouteInfo> records = queryRecords(tenantNo, custNo, routeTypCd,
                    statusCd);

            if (records.isEmpty()) {
                response.returnCode = 20000;
                response.returnMessage = "未找到客户账户路由信息";
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

    private List<CustAcctRouteInfo> queryRecords(String tenantNo, String custNo,
            String routeTypCd, String statusCd) throws SQLException {
        StringBuilder sql = new StringBuilder(
                "SELECT CUST_NO, AFS_PRODT_NO, BASE_PRODT_NO, MAIN_ACCT_NO, "
                + "OPER_TYP_CD, RELA_SEQ_NO, ROUTE_TYP_CD, ROUTE_VAL, VALID_FLG "
                + "FROM CUST_ACCT_INFO "
                + "WHERE TENANT_NO = ? AND CUST_NO = ? AND ROUTE_TYP_CD = ?");

        boolean hasStatus = statusCd != null && !statusCd.trim().isEmpty();
        if (hasStatus) {
            sql.append(" AND VALID_FLG = ?");
        }

        try (PreparedStatement ps = connection.prepareStatement(sql.toString())) {
            ps.setString(1, tenantNo);
            ps.setString(2, custNo);
            ps.setString(3, routeTypCd);
            if (hasStatus) {
                ps.setString(4, statusCd);
            }

            List<CustAcctRouteInfo> records = new ArrayList<>();
            try (ResultSet rs = ps.executeQuery()) {
                while (rs.next()) {
                    CustAcctRouteInfo info = new CustAcctRouteInfo();
                    info.custNo = rs.getString("CUST_NO");
                    info.afsProDtNo = rs.getString("AFS_PRODT_NO");
                    info.baseProDtNo = rs.getString("BASE_PRODT_NO");
                    info.mainAcctNo = rs.getString("MAIN_ACCT_NO");
                    info.operTypCd = rs.getString("OPER_TYP_CD");
                    info.relaSeqNo = rs.getString("RELA_SEQ_NO");
                    info.routeTypCd = rs.getString("ROUTE_TYP_CD");
                    info.routeVal = rs.getString("ROUTE_VAL");
                    info.validFlg = rs.getString("VALID_FLG");
                    records.add(info);
                }
            }
            return records;
        }
    }
}

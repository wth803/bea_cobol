import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

/**
 * Query Personal Customer Channel Transaction Command Service
 * (对私客户渠道交易控制查询服务).
 *
 * <p>Implements personal customer channel transaction control command query by
 * customer number and tenant number.
 *
 * <p>Converted from COBOL program QURYPERCUSTCHNLTXNCOMMOND
 * (12.QuryPerCustChnlTxnCommond.cbl).
 */
public class QueryPersonalCustomerChannelTxnCommandService {

    private final Connection connection;

    public QueryPersonalCustomerChannelTxnCommandService(Connection connection) {
        this.connection = connection;
    }

    /**
     * A single channel transaction control record.
     */
    public static class CustTxnChnlInfo {
        private BigDecimal yrAccmMaxTxAmt;
        private BigDecimal monAccmMaxTxAmt;
        private String pmitTerminalTypCd;
        private String lmtTypCd;
        private BigDecimal dayAccmMaxTxAmt;
        private int monAccmMaxTxStkCnt;
        private int dayAccmMaxTxStkCnt;
        private int yrAccmMaxTxStkCnt;
        private BigDecimal sglTxHighAmt;
        private BigDecimal sglTxLowestAmt;
        private int qtAccmMaxTxStkCnt;
        private BigDecimal qtAccmMaxTxAmt;
        private String custNo;
        private String rsn;
        private String validFlg;

        public BigDecimal getYrAccmMaxTxAmt() { return yrAccmMaxTxAmt; }
        public BigDecimal getMonAccmMaxTxAmt() { return monAccmMaxTxAmt; }
        public String getPmitTerminalTypCd() { return pmitTerminalTypCd; }
        public String getLmtTypCd() { return lmtTypCd; }
        public BigDecimal getDayAccmMaxTxAmt() { return dayAccmMaxTxAmt; }
        public int getMonAccmMaxTxStkCnt() { return monAccmMaxTxStkCnt; }
        public int getDayAccmMaxTxStkCnt() { return dayAccmMaxTxStkCnt; }
        public int getYrAccmMaxTxStkCnt() { return yrAccmMaxTxStkCnt; }
        public BigDecimal getSglTxHighAmt() { return sglTxHighAmt; }
        public BigDecimal getSglTxLowestAmt() { return sglTxLowestAmt; }
        public int getQtAccmMaxTxStkCnt() { return qtAccmMaxTxStkCnt; }
        public BigDecimal getQtAccmMaxTxAmt() { return qtAccmMaxTxAmt; }
        public String getCustNo() { return custNo; }
        public String getRsn() { return rsn; }
        public String getValidFlg() { return validFlg; }
    }

    /**
     * Response object returned by {@link #query}.
     */
    public static class QueryChannelTxnCommandResponse {
        private int returnCode;
        private String returnMessage;
        private List<CustTxnChnlInfo> records = new ArrayList<>();

        public int getReturnCode() { return returnCode; }
        public String getReturnMessage() { return returnMessage; }
        public List<CustTxnChnlInfo> getRecords() { return records; }
    }

    /**
     * Queries channel transaction control commands for a personal customer.
     *
     * @param custNo   customer number (客户编号)
     * @param tenantNo tenant number (租户号)
     * @return {@link QueryChannelTxnCommandResponse} with result code, message, and
     *         matching control records
     */
    public QueryChannelTxnCommandResponse query(String custNo, String tenantNo) {
        QueryChannelTxnCommandResponse response = new QueryChannelTxnCommandResponse();
        response.returnCode = 0;

        // 1) Validate required parameters
        if (custNo == null || custNo.trim().isEmpty()) {
            response.returnCode = 1001;
            response.returnMessage = "客户编号不能为空";
            return response;
        }
        if (tenantNo == null || tenantNo.trim().isEmpty()) {
            response.returnCode = 1002;
            response.returnMessage = "租户号不能为空";
            return response;
        }

        try {
            // 2) Query channel transaction control records
            List<CustTxnChnlInfo> records = queryRecords(custNo, tenantNo);

            if (records.isEmpty()) {
                response.returnCode = 20000;
                response.returnMessage = "未找到客户交易渠道控制信息";
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

    private List<CustTxnChnlInfo> queryRecords(String custNo, String tenantNo)
            throws SQLException {
        String sql = "SELECT YR_ACCM_MAX_TX_AMT, MON_ACCM_MAX_TX_AMT, PMIT_TERMINAL_TYP_CD, "
                + "LMT_TYP_CD, DAY_ACCM_MAX_TX_AMT, MON_ACCM_MAX_TX_STKCNT, "
                + "DAY_ACCM_MAX_TX_STKCNT, YR_ACCM_MAX_TX_STKCNT, SGL_TX_HIGH_AMT, "
                + "SGL_TX_LOWEST_AMT, QT_ACCM_MAX_TX_STKCNT, QT_ACCM_MAX_TX_AMT, "
                + "CUST_NO, RSN, VALID_FLG "
                + "FROM CUST_CHNL_TXN_COMMOND "
                + "WHERE CUST_NO = ? AND TENANT_NO = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, custNo);
            ps.setString(2, tenantNo);

            List<CustTxnChnlInfo> records = new ArrayList<>();
            try (ResultSet rs = ps.executeQuery()) {
                while (rs.next()) {
                    CustTxnChnlInfo info = new CustTxnChnlInfo();
                    info.yrAccmMaxTxAmt = rs.getBigDecimal("YR_ACCM_MAX_TX_AMT");
                    info.monAccmMaxTxAmt = rs.getBigDecimal("MON_ACCM_MAX_TX_AMT");
                    info.pmitTerminalTypCd = rs.getString("PMIT_TERMINAL_TYP_CD");
                    info.lmtTypCd = rs.getString("LMT_TYP_CD");
                    info.dayAccmMaxTxAmt = rs.getBigDecimal("DAY_ACCM_MAX_TX_AMT");
                    info.monAccmMaxTxStkCnt = rs.getInt("MON_ACCM_MAX_TX_STKCNT");
                    info.dayAccmMaxTxStkCnt = rs.getInt("DAY_ACCM_MAX_TX_STKCNT");
                    info.yrAccmMaxTxStkCnt = rs.getInt("YR_ACCM_MAX_TX_STKCNT");
                    info.sglTxHighAmt = rs.getBigDecimal("SGL_TX_HIGH_AMT");
                    info.sglTxLowestAmt = rs.getBigDecimal("SGL_TX_LOWEST_AMT");
                    info.qtAccmMaxTxStkCnt = rs.getInt("QT_ACCM_MAX_TX_STKCNT");
                    info.qtAccmMaxTxAmt = rs.getBigDecimal("QT_ACCM_MAX_TX_AMT");
                    info.custNo = rs.getString("CUST_NO");
                    info.rsn = rs.getString("RSN");
                    info.validFlg = rs.getString("VALID_FLG");
                    records.add(info);
                }
            }
            return records;
        }
    }
}

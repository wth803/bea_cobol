#!/usr/bin/env python3
"""
COBOL Procedure Division Analyzer
Analyzes COBOL source files and generates structured JSON describing business logic.
"""

import os
import re
import json


COBOL_DIR = os.path.join(os.path.dirname(__file__), "cobol")

# Curated metadata per COBOL file based on domain analysis.
# Keys correspond to the COBOL file base name (without leading number and extension).
FILE_METADATA = {
    "ConsolidationCust": {
        "element_name": "ConsolidationCust",
        "element_name_cn": "客户归并服务",
        "description": "Implement customer consolidation by migrating account routing from source to target customer",
        "description_cn": "实现客户归并，将源客户账户路由信息迁移至目标客户",
        "program_id": "CustomerConsolidationService",
    },
    "ConsolidationCustByAcctNo": {
        "element_name": "ConsolidationCustByAcctNo",
        "element_name_cn": "按账号客户归并服务",
        "description": "Implement customer consolidation by updating routing records for a specified account list",
        "description_cn": "实现按账号列表执行客户归并，批量更新路由记录至目标客户",
        "program_id": "CustomerConsolidationByAcctService",
    },
    "CreatePerCustInfo": {
        "element_name": "CreatePerCustInfo",
        "element_name_cn": "个人客户开立服务（简版）",
        "description": "Implement personal customer creation with deduplication and ID-card information extraction",
        "description_cn": "实现个人客户开立，含重复校验及身份证信息自动提取",
        "program_id": "CreatePersonalCustomerService",
    },
    "CrtPerCustInfo": {
        "element_name": "CrtPerCustInfo",
        "element_name_cn": "个人客户开立服务（完整版）",
        "description": "Implement full personal customer creation with comprehensive field validation and deduplication",
        "description_cn": "实现完整个人客户开立，含全字段校验与重复性检查",
        "program_id": "CreatePersonalCustomerFullService",
    },
    "MgmtCustAcctInfo": {
        "element_name": "MgmtCustAcctInfo",
        "element_name_cn": "客户账户路由管理服务",
        "description": "Manage customer account routing information supporting add, modify, and delete operations",
        "description_cn": "管理客户账户路由信息，支持新增、修改和删除三种操作",
        "program_id": "CustomerAccountInfoManageService",
    },
    "MgmtPerCustlnfo": {
        "element_name": "MgmtPerCustlnfo",
        "element_name_cn": "个人客户信息维护服务",
        "description": "Manage personal customer basic and extended information update with partial-update support",
        "description_cn": "管理个人客户基础与扩展信息的更新，支持字段级局部修改",
        "program_id": "PersonalCustomerInfoManageService",
    },
    "QuryCustAcctInfoByCustAcct": {
        "element_name": "QuryCustAcctInfoByCustAcct",
        "element_name_cn": "按账号查询客户账户路由服务",
        "description": "Implement customer account routing information query by route value and route type",
        "description_cn": "实现按路由值和路由类型查询客户账户路由信息",
        "program_id": "QueryCustomerAccountByAcctService",
    },
    "QuryCustAcctInfoByCustNo": {
        "element_name": "QuryCustAcctInfoByCustNo",
        "element_name_cn": "按客户号查询账户路由服务",
        "description": "Implement customer account routing information query by customer number and route type",
        "description_cn": "实现按客户号和路由类型查询客户账户路由信息",
        "program_id": "QueryCustomerAccountByCustNoService",
    },
    "QuryCustInfo": {
        "element_name": "QuryCustInfo",
        "element_name_cn": "客户基本信息查询服务",
        "description": "Implement customer basic information query combining customer profile and risk level",
        "description_cn": "实现客户基本信息查询，综合返回客户档案与风险等级信息",
        "program_id": "QueryCustomerInfoService",
    },
    "QuryCustType": {
        "element_name": "QuryCustType",
        "element_name_cn": "客户类型查询服务",
        "description": "Implement customer type code query by customer number",
        "description_cn": "实现按客户号查询客户类型代码",
        "program_id": "QueryCustomerTypeService",
    },
    "QuryOvsCashWithdrReCtrFlg": {
        "element_name": "QuryOvsCashWithdrReCtrFlg",
        "element_name_cn": "境外取现控制标志查询服务",
        "description": "Implement overseas cash withdrawal blacklist flag query by customer number",
        "description_cn": "实现按客户号查询境外取现黑名单控制标志",
        "program_id": "QueryOverseasCashWithdrawalControlFlagService",
    },
    "QuryPerCustChnlTxnCommond": {
        "element_name": "QuryPerCustChnlTxnCommond",
        "element_name_cn": "对私客户渠道交易控制查询服务",
        "description": "Implement personal customer channel transaction control command query",
        "description_cn": "实现对私客户渠道交易控制指令的查询",
        "program_id": "QueryPersonalCustomerChannelTxnCommandService",
    },
    "QuryPerCustInfoByCustNo": {
        "element_name": "QuryPerCustInfoByCustNo",
        "element_name_cn": "按客户号查询个人客户信息服务",
        "description": "Implement comprehensive personal customer information query by customer number",
        "description_cn": "实现按客户号综合查询个人客户基础及扩展信息",
        "program_id": "QueryPersonalCustomerInfoByCustNoService",
    },
    "QuryPerCustNameListInfoByCustNo": {
        "element_name": "QuryPerCustNameListInfoByCustNo",
        "element_name_cn": "对私客户名单信息查询服务",
        "description": "Implement personal customer watchlist and name-list information query by customer number",
        "description_cn": "实现按客户号查询对私客户名单信息",
        "program_id": "QueryPersonalCustomerNameListService",
    },
    "QuryPerCustRiskLevel": {
        "element_name": "QuryPerCustRiskLevel",
        "element_name_cn": "对私客户风险等级查询服务",
        "description": "Implement personal customer risk level and risk assessment result query",
        "description_cn": "实现对私客户风险等级及风险评估结果的查询",
        "program_id": "QueryPersonalCustomerRiskLevelService",
    },
    "QurySignRelationInfo": {
        "element_name": "QurySignRelationInfo",
        "element_name_cn": "客户签约关系查询服务",
        "description": "Implement customer signing relationship information query by customer number",
        "description_cn": "实现按客户号查询客户签约关系信息",
        "program_id": "QuerySignRelationInfoService",
    },
}


def extract_file_key(filename: str) -> str:
    """Strip leading number prefix and .cbl extension from a filename."""
    base = os.path.splitext(filename)[0]
    return re.sub(r"^\d+\.", "", base)


def find_procedure_division_start(lines: list[str]) -> int:
    """Return 1-based line number where PROCEDURE DIVISION begins."""
    for i, line in enumerate(lines, start=1):
        if re.search(r"\bPROCEDURE\s+DIVISION\b", line, re.IGNORECASE):
            return i
    return 1


_SQLCODE_LOOKAHEAD_LINES = 6
_END_IF_SEARCH_LINES = 10
_IF_BLOCK_SEARCH_LINES = 8
_EVALUATE_BLOCK_SEARCH_LINES = 15
_DEFAULT_SNIPPET_LINES = 10
_RANGE_MERGE_TOLERANCE = 2


def collect_key_snippets(lines: list[str], proc_start: int) -> list[str]:
    """
    Identify up to 5 key line-range snippets from the PROCEDURE DIVISION:
      1. SQL blocks (EXEC SQL … END-EXEC)
      2. IF/error-handling blocks adjacent to SQL
      3. Data movement tied to SQL operations

    Returns a list of strings in the form "start-end" (1-based, inclusive).
    """
    n = len(lines)
    # Collect ranges of significant blocks.
    raw_ranges: list[tuple[int, int]] = []

    i = proc_start - 1  # convert 1-based proc_start to 0-based index

    while i < n:
        # --- SQL block ---
        if re.search(r"\bEXEC\s+SQL\b", lines[i], re.IGNORECASE):
            start = i
            end = i
            for j in range(i, n):
                end = j
                if re.search(r"\bEND-EXEC\b", lines[j], re.IGNORECASE):
                    break
            # Extend to include adjacent IF SQLCODE handling
            k = end + 1
            while k < n and k <= end + _SQLCODE_LOOKAHEAD_LINES:
                stripped = lines[k].strip().upper()
                if stripped.startswith("IF SQLCODE") or stripped.startswith("IF WS-RESP-CODE"):
                    # scan to END-IF
                    m = k
                    for l in range(k, min(k + _END_IF_SEARCH_LINES, n)):
                        m = l
                        if re.search(r"\bEND-IF\b", lines[l], re.IGNORECASE):
                            break
                    end = max(end, m)
                    k = m + 1
                else:
                    break
            raw_ranges.append((start + 1, end + 1))  # convert to 1-based
            i = end + 1
            continue

        # --- Standalone IF/error handling in procedure division ---
        if re.search(r"\bIF\b.*(NOT\s+=\s+0|=\s+SPACES|LOW-VALUES|F[0-9]{5})", lines[i], re.IGNORECASE):
            start = i
            end = i
            for j in range(i, min(i + _IF_BLOCK_SEARCH_LINES, n)):
                end = j
                if re.search(r"\bEND-IF\b|\bGO\s+TO\b", lines[j], re.IGNORECASE):
                    break
            raw_ranges.append((start + 1, end + 1))
            i = end + 1
            continue

        # --- EVALUATE blocks ---
        if re.search(r"\bEVALUATE\b", lines[i], re.IGNORECASE):
            start = i
            end = i
            for j in range(i, min(i + _EVALUATE_BLOCK_SEARCH_LINES, n)):
                end = j
                if re.search(r"\bEND-EVALUATE\b", lines[j], re.IGNORECASE):
                    break
            raw_ranges.append((start + 1, end + 1))
            i = end + 1
            continue

        i += 1

    if not raw_ranges:
        # Fallback: include PROCEDURE DIVISION header and first few lines
        raw_ranges.append((proc_start, min(proc_start + _DEFAULT_SNIPPET_LINES, n)))

    # Deduplicate overlapping ranges, keep at most 5.
    merged: list[tuple[int, int]] = []
    for start, end in sorted(raw_ranges):
        if merged and start <= merged[-1][1] + _RANGE_MERGE_TOLERANCE:
            merged[-1] = (merged[-1][0], max(merged[-1][1], end))
        else:
            merged.append((start, end))

    # Take 5 most representative (first covers init, then SQL-heavy ones).
    selected = merged[:5]

    return [f"{s}-{e}" for s, e in selected]


def analyze_file(filepath: str) -> dict:
    """Analyze a single COBOL file and return the analysis record."""
    filename = os.path.basename(filepath)
    key = extract_file_key(filename)

    with open(filepath, encoding="utf-8", errors="replace") as f:
        lines = f.readlines()

    proc_start = find_procedure_division_start(lines)
    source_lines = collect_key_snippets(lines, proc_start)

    meta = FILE_METADATA.get(key, {})
    return {
        "element_name": meta.get("element_name", key),
        "element_name_cn": meta.get("element_name_cn", key),
        "description": meta.get("description", ""),
        "description_cn": meta.get("description_cn", ""),
        "program_id": meta.get("program_id", key + "Service"),
        "source_lines": source_lines,
    }


def main():
    cobol_files = sorted(
        f for f in os.listdir(COBOL_DIR) if f.endswith(".cbl")
    )

    results = []
    for filename in cobol_files:
        filepath = os.path.join(COBOL_DIR, filename)
        record = analyze_file(filepath)
        results.append(record)
        print(f"Analyzed: {filename}")

    output_path = os.path.join(os.path.dirname(__file__), "cobol_analysis.json")
    with open(output_path, "w", encoding="utf-8") as f:
        json.dump(results, f, ensure_ascii=False, indent=4)

    print(f"\nAnalysis written to: {output_path}")
    print(json.dumps(results, ensure_ascii=False, indent=4))


if __name__ == "__main__":
    main()

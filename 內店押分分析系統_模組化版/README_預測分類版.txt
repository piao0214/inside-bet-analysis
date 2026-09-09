內店押分分析系統 - 預測分類版

新增功能：
1. 新增「預測分類」分頁。
2. 分別產生「遊戲預測分類」與「店家預測分類」兩個表格。
3. 每個表格同時列出未來第 1 期與第 2 期。
4. 分類規則使用兩期預測圖的 85% CI：
   - 上漲：85% CI 下界 > 0
   - 正常：85% CI 包含 0
   - 下跌：85% CI 上界 < 0
5. 遊戲分類：逐一對全部遊戲做預測（跨全部店家）。
6. 店家分類：逐一對全部店家做預測（含該店全部遊戲）。
7. 資料不足或建模失敗的項目不塞進三分類，會在表格下方列出提醒。
8. 保留本機 Ollama / Posit Connect Cloud Ollama Cloud 自動切換版本。

部署到 Posit Connect Cloud 前：
請在專案資料夾重新執行

rsconnect::writeManifest(
  appDir = ".",
  appPrimaryDoc = "app.R",
  appMode = "shiny"
)

然後更新 GitHub 的：
- app.R
- manifest.json
- R/04_ollama.R
- R/06_ui.R

注意：
這個 ZIP 沒有替你產生新的 manifest.json，因為 manifest 應在你的 Windows R 環境中重新產生。

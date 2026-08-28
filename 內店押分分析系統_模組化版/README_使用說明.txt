內店押分分析系統 - 模組化版

【如何執行】
1. 解壓縮整個資料夾，不要只拿 app.R。
2. 建議用 RStudio 開啟「內店押分分析系統.Rproj」。
3. 開啟 app.R。
4. 按右上角 Run App。
5. 上傳 Excel，設定年月，按「讀取資料」。
6. 選店家 / 遊戲後查看分析頁籤。

【頁籤順序】
1. 兩期預測
2. LOG RETURN測試
3. RETURN測試
4. AI分析

【資料夾結構】
app.R                         主程式，只負責串接 UI / Reactive / Output
R/01_utils.R                  共用小工具、遊戲名稱整理、缺值補平均
R/02_excel.R                  Excel 讀取、店家拆分、資料彙整
R/03_forecast.R               ARIMA、LOG RETURN、RETURN、兩期預測
R/04_ollama.R                 Ollama API 與 AI Prompt
R/05_plots.R                  三張圖的 ggplot 程式
R/06_ui.R                     Shiny UI 與頁籤順序

【重要】
- app.R 會用 source() 自動載入 R/ 內的 function 檔。
- 所以 R 資料夾不可刪除，也不要只把 app.R 單獨搬走。
- Ollama 預設網址為 http://localhost:11434。
- 沒有 Ollama 仍可使用三個圖表頁籤，只有 AI 分析無法呼叫模型。
- RETURN 定義為 exp(LOG RETURN) - 1，因此 0% 為持平、正值為上漲、負值為下跌。

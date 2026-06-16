install.packages("readxl")
library(readxl)
library(tidyverse)

######
df <- read_excel("perfect.xlsx")
df_ratio <- df %>%
  mutate(
    # グループ別の件数
    sector_primary       = agri_forestry + fisheries,
    sector_construction  = construction,
    sector_manufacturing = food_manufacturing + textile_apparel + machinery_metal,
    
    # 構成比の分母：3グループ＋other を全部足す
    total_plan = sector_primary + sector_construction + sector_manufacturing + other,
    
    # 構成比（0〜1）
    share_primary       = sector_primary      / total_plan,
    share_construction  = sector_construction / total_plan,
    share_manufacturing = sector_manufacturing / total_plan
  ) %>%
  # 3. 元の業種列＋中間列＋other を削除して、share_* だけ残す
  select(
    -agri_forestry, -fisheries, -construction,
    -food_manufacturing, -textile_apparel, -machinery_metal,
    -other,
    -sector_primary, -sector_construction, -sector_manufacturing,
    -total_plan
  )
#空間重み行列を作る
install.packages("geosphere")
library(geosphere)
library(writexl)
df2 <- read_excel("Citylatlongi.xlsx")
# 2. 経度・緯度行列（geosphere は lon, lat の順）
coords <- as.matrix(df2[, c("City_longitude", "City_latitude")])
dist_mat <- distm(coords, fun = distHaversine)
rownames(dist_mat) <- df2$Prefecture
colnames(dist_mat) <- df2$Prefecture
diag(dist_mat) <- NA
W <- 1 / dist_mat
W[is.na(W)] <- 0
row_sums <- rowSums(W)
W_rowstd <- W
W_rowstd[row_sums > 0, ] <- W[row_sums > 0, ] / row_sums[row_sums > 0]

W_df <- as.data.frame(W_rowstd)
W_df <- cbind(Prefecture = rownames(W_rowstd), W_df)
write_xlsx(df_ratio, "perfect_ratio.xlsx")

#ええと技能実習受け入れ企業の違反件数　集計値もう一回つくる
df3 <- read_excel("master violations.xlsx")
# 2. 集計処理
df_agg <- df3 %>%
  mutate(Prefecture = str_trim(Prefecture)) %>%
  
  # 集計
  count(Prefecture, Year, name = "violation_count") %>%
  
  # 年 2020〜2024 をゼロ埋めする
  complete(Prefecture,
           Year = 2020:2024,
           fill = list(violation_count = 0)) %>%
  
  # 並べ替え
  arrange(Prefecture, Year)

df4 <- read_excel("book2.xlsx")

#全部結合 book2がperfect_ratioに日本語教室の推移と可住地面積を入れた物で　そこに違反企業行集計をして　それマージした　
df5 <- df4 %>%
  left_join(df_agg, by = c("Prefecture","Year"))
write_xlsx(df5, "Final.xlsx")
##### final.xlsx 

library(fixest)
library(car)
install.packages("estimatr")
library(estimatr)
df <- read_excel("Final.xlsx")
df <- df %>%
  mutate(
    jp_school_density_area = jp_school_num / Inhabitable_area
  )
model_formula <- disappear_ratio ~ 
  foreigner_ratio + job_offer_ratio_avg + 
  share_primary + share_construction +
  share_manufacturing + min_salary + 
  jp_school_density_area + violation_count

fe_model <- lm_robust(
  formula        = model_formula,
  data           = df,
  fixed_effects  = ~ Prefecture + Year,  # 県＋年の固定効果
  se_type        = "HC2"                 # ロバストSE（好みでOK）
)
summary(fe_model)

# VIF用：固定効果なしの通常回帰
lm_for_vif <- lm(
  disappear_ratio ~ 
    foreigner_ratio + job_offer_ratio_avg + 
    share_primary + share_construction +
    share_manufacturing + min_salary + 
    jp_school_density_area + violation_count,
  data = df
)

vif(lm_for_vif)

# フルランクチェック
X <- df %>%
  dplyr::select(
    foreigner_ratio, job_offer_ratio_avg,
    share_primary, share_construction, share_manufacturing,
    min_salary, jp_school_density_area, violation_count
  ) %>%
  as.matrix()

qr_rank <- qr(X)$rank
rank_expected <- ncol(X)

qr_rank
rank_expected

# 説明変数だけ cbindをする
reg_vars <- c(
  "foreigner_ratio", "job_offer_ratio_avg",
  "share_primary", "share_construction", "share_manufacturing",
  "min_salary", "jp_school_density_area", "violation_count"
)

X_main <- as.matrix(df[ , reg_vars])

# 固定効果のダミー行列（切片なし）
FE_mat <- model.matrix(~ Prefecture + Year - 1, data = df)

# cbind で全部まとめる
X_all <- cbind(X_main, FE_mat)

qr_rank_all    <- qr(X_all)$rank
rank_expected_all <- ncol(X_all)

qr_rank_all
rank_expected_all　　#どっちでもフルランクでした
#####
library(modelsummary)
df <- df %>%
  mutate(
    jp_school_density_area = jp_school_num / Inhabitable_area
  )
model_formula <- disappear_ratio ~ 
  foreigner_ratio + job_offer_ratio_avg + 
  share_primary + share_construction +
  share_manufacturing + min_salary + 
  jp_school_density_area + violation_count
# モデル1: プーリング（FEなし）+ クラスターロバスト標準誤差
m1 <- lm_robust(
  formula = model_formula,
  data    = df,
  clusters = Prefecture,  # ←これを追加
  se_type = "CR2"
)

# モデル2: 県FEのみ + クラスターロバスト標準誤差
m2 <- lm_robust(
  formula       = model_formula,
  data          = df,
  fixed_effects = ~ Prefecture,
  clusters      = Prefecture,  # ←これを追加
  se_type       = "CR2"
)

# モデル3: 年FEのみ + クラスターロバスト標準誤差
m3 <- lm_robust(
  formula       = model_formula,
  data          = df,
  fixed_effects = ~ Year,
  clusters      = Prefecture,  # ←これを追加（年FEモデルでも、誤差の相関は「県単位」で考えるのが普通です）
  se_type       = "CR2"
)

# モデル4: 県FE + 年FE（TWFE） + クラスターロバスト標準誤差
m4 <- lm_robust(
  formula       = model_formula,
  data          = df,
  fixed_effects = ~ Prefecture + Year,
  clusters      = Prefecture,  # ←これを追加
  se_type       = "CR2"
)
install.packages("webshot2")
library(gt)
library(webshot2)
# 1. モデルのリスト化（名前をつける）
models <- list(
  "(1) Pooled OLS"      = m1,
  "(2) Pref FE"         = m2,
  "(3) Year FE"         = m3,
  "(4) TWFE "     = m4
)

# 2. 表に表示する統計量の設定（シンプルにする）
gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "Observations",  0,
  "r.squared", "R-squared",     3,
  "adj.r.squared", "Adj. R2",   3
)

# 3. 画像として出力（ここがポイント！）
modelsummary(
  models,
  fmt = 3,                                      # 小数点3桁
  stars = c('*' = .1, '**' = .05, '***' = .01), # 星の基準
  gof_map = gm,                                 # 統計量の指定
  coef_rename = c(                              # 変数名をきれいな英語に
    "foreigner_ratio"      = "Foreigner Ratio",
    "job_offer_ratio_avg"  = "Job Offer Ratio",
    "share_manufacturing"  = "Share of Manufacturing",
    "share_construction"   = "Share of Construction",
    "share_primary"        = "Share of Primary Sector",
    "min_salary"           = "Minimum Wage (t-1)",
    "jp_school_density_area" = "JP School Density(t-1)",
    "violation_count"      = "Labor Violations"
  ),
  title = "Table 1: Regression Results of Disappearance Rate", # タイトル
  output = "Result_Table.png"  # ★ここで画像ファイル名を指定！
)
vars_df <- df[, c(
  "disappear_ratio",        # ← ここを修正しました！
  "foreigner_ratio",
  "job_offer_ratio_avg",
  "share_manufacturing",
  "share_construction",
  "share_primary",
  "min_salary",
  "jp_school_density_area",
  "violation_count"
)]

# 2. 変数名をきれいな英語に変える（先ほどと同じ要領）
colnames(vars_df) <- c(
  "Disappearance Rate",     # 失踪率
  "Foreigner Ratio",        # 外国人比率
  "Job Offer Ratio",        # 有効求人倍率
  "Share of Manufacturing", # 製造業比率
  "Share of Construction",  # 建設業比率
  "Share of Primary Sector",# 第1次産業比率
  "Minimum Wage",           # 最低賃金
  "JP School Density",      # 日本語教室密度
  "Labor Violations"        # 違反企業数
)

# 3. 記述統計表を作成してWordに出力
datasummary_skim(
  vars_df,
  fmt = 2,                      # 小数点2桁まで表示
  title = "Table 1: Descriptive Statistics", 
  output = "Descriptive_Table.png" # Wordで保存
)




## ==== SDM ====
#空間重み行列を作る
install.packages("geosphere")
library(geosphere)
df <- read_excel("Final.xlsx")
df <- df %>%
  mutate(
    jp_school_density_area = jp_school_num / Inhabitable_area
  )
df2 <- read_excel("Citylatlongi.xlsx")
df2 <- df2 %>% arrange(Prefecture)
master_order <- df2$Prefecture

df <- df %>%
  mutate(Prefecture = factor(Prefecture, levels = master_order))
df <- df %>% arrange(Prefecture, Year) # これでdf１とdf２の都道府県の順番がそろった
# 2. 経度・緯度行列（geosphere は lon, lat の順）
install.packages(c("spdep", "splm"))
library(spdep)
library(splm)
# 1. 座標データの準備（すでにある coords を使います）
coords <- as.matrix(df2[, c("City_longitude", "City_latitude")])

# 2. k-近傍隣接リストの作成
# k = 4 (近い順に4つの県とつながる) に設定します。
# 日本の都道府県の場合、平均隣接数が4〜5なので、k=4 は非常に妥当です。
k_nn <- knearneigh(coords, k = 4)

# 3. nb（neighbor）オブジェクトに変換
knn_nb <- knn2nb(k_nn)

# 4. 重み行列（listw）に変換
# style = "W" で行正規化（足して1になるようにする）します
listw_knn <- nb2listw(knn_nb, style = "W")

# --- 確認 ---
print(listw_knn)



# plotでつながりを見てみる（地図っぽく表示されます）
plot(knn_nb, coords, col = "red", pch = 20, cex = 0.5)
title("k-Nearest Neighbors (k=4) Connections")
install.packages(c("maps", "mapdata"))
library(maps)
library(mapdata)

# 2. プロットの描画
# (A) 日本地図（下地）を描く
map("japan", col = "grey90", fill = TRUE, border = "white")

# (B) その上に k-NN のネットワークを重ねる (add = TRUE)
plot(knn_nb, coords, 
     add = TRUE,      # ★地図の上に重ねるための重要オプション
     col = "red",     # 線の色
     lwd = 1,         # 線の太さ
     pch = 20,        # 点の形
     cex = 0.8)       # 点の大きさ

# (C) タイトル
title("k-Nearest Neighbors (k=4) Network on Japan Map")
png("Japan_Network_Map.png", width = 1600, height = 1600, res = 200)

# 2. 余白を消す（これで地図が画面いっぱいに広がります）
par(mar = c(0, 0, 2, 0)) 

# 3. プロットの描画（さっきと同じコード）
map("japan", col = "grey90", fill = TRUE, border = "white")
plot(knn_nb, coords, 
     add = TRUE, 
     col = "red", 
     lwd = 1.5,       # 線を少し太くする
     pch = 20, 
     cex = 1.2)       # 点を少し大きくする

title("k-Nearest Neighbors (k=4) Network", cex.main = 2) # タイトルも大きく

# 4. 保存完了
dev.off()



# ==== 説明変数の空間ラグWXを作る ====
# 1. k-NN重み行列を行列形式に変換（計算用）
W_mat <- listw2mat(listw_knn)

# 2. 空間ラグ変数を作成する関数
calc_lag <- function(x, w) as.vector(w %*% x)

#モーランI検定をしよう



years <- sort(unique(df$Year))

moran_results <- lapply(years, function(yr) {
  df_year <- df[df$Year == yr, ] %>% arrange(Prefecture)
  
  mi <- moran.test(df_year$disappear_ratio, listw_knn)
  
  list(
    year = yr,
    statistic = mi$statistic,
    p_value = mi$p.value,
    expectation = mi$estimate["E.I"],
    variance = mi$estimate["Var.I"]
  )
})

moran_results
library(fixest)
fe_model <- feols(
  disappear_ratio ~ 
    foreigner_ratio + job_offer_ratio_avg +
    share_primary + share_construction + share_manufacturing +
    min_salary + jp_school_density_area + violation_count |
    Prefecture,
  data = df
)
df$resid_fe <- resid(fe_model)

# 年ごとの残差の Moran’s I
years <- sort(unique(df$Year))
moran_resid_results <- lapply(years, function(yr) {
  df_year <- df[df$Year == yr, ] %>% arrange(Prefecture)
  moran.test(df_year$resid_fe, listw_knn)
})
moran_resid_results 

moran_table <- lapply(1:length(moran_resid_results), function(i) {
  mi <- moran_resid_results[[i]]
  data.frame(
    Year = years[i],
    Moran_I = unname(mi$estimate["Moran I statistic"]),
    Expected = unname(mi$estimate["E.I"]),
    Variance = unname(mi$estimate["Var.I"]),
    Z_value = unname(mi$statistic),
    P_value = mi$p.value
  )
}) %>% bind_rows()

moran_table
#最低賃金の対数化を忘れるな！！！
df <- df %>%
  mutate(
    ln_min_salary = log(min_salary)   # 自然対数 ln
  )


library(panelsummary)
panelsummary(
  df,
  columns = c("disappear_ratio", "foreigner_ratio", "job_offer_ratio_avg",
              "share_primary", "share_construction", "share_manufacturing",
              "ln_min_salary", "jp_school_density_area", "violation_count"),
  panel_identifier = c("Prefecture", "Year")
)




# 3. データフレームに追加（年ごとに計算！）
df_sdm <- df %>%
  group_by(Year) %>%             # 年ごとに区切って計算
  mutate(
    # 主要な変数の「お隣さん版（WX）」を作る
    W_share_manufacturing    = calc_lag(share_manufacturing, W_mat),
    W_share_construction     = calc_lag(share_construction, W_mat),
    W_share_primary          = calc_lag(share_primary, W_mat),
    W_ln_min_salary             = calc_lag(ln_min_salary, W_mat),
    W_jp_school_density_area = calc_lag(jp_school_density_area, W_mat),
    W_violation_count        = calc_lag(violation_count, W_mat),
    W_job_offer_ratio_avg    = calc_lag(job_offer_ratio_avg, W_mat),
    W_foreigner_ratio        = calc_lag(foreigner_ratio, W_mat) 
  ) %>%
  ungroup()

# 数式の定義： Y ~ X + WX 　　SDM式　WXをすべて含める
sdm_formula <- disappear_ratio ~ 
  # (1) 自県の要因 (X)
  share_manufacturing + share_construction + share_primary +
  ln_min_salary + jp_school_density_area + violation_count +
  foreigner_ratio + job_offer_ratio_avg +
  # (2) 隣県の要因 (WX)
  W_share_manufacturing + W_share_construction + W_share_primary +
  W_ln_min_salary + W_jp_school_density_area + W_violation_count +
  W_job_offer_ratio_avg + W_foreigner_ratio

# 推定実行
sdm_result <- spml(
  formula = sdm_formula,
  data    = df_sdm,
  listw   = listw_knn,       # k-NNの重み
  model   = "within",        # 固定効果モデル
  effect  = "individual",    # 都道府県固定効果
  spatial.error = "none",    # エラーの相関はなし
  lag     = TRUE             # ★従属変数の空間ラグ(rho)を入れる
)
sdm_result <- spml(
  formula = sdm_formula,
  data    = df_sdm,
  listw   = listw_knn,       # k-NNの重み
  model   = "within",        # 固定効果モデル
  effect  = "twoways",    # 双方向固定効果
  spatial.error = "none",    # エラーの相関はなし
  lag     = TRUE             # ★従属変数の空間ラグ(rho)を入れる
)
# 結果の表示
summary(sdm_result)#ρはlamdaで表示されている　固定効果入りSDMではlamdaとして表示される名残があるらしく

#空間計量経済学では回帰結果の係数をそのまま解釈してはいけない
# 1. インパクトの計算
# listw: 空間重み行列, time: パネルの期間数（5年なら5）
# R: シミュレーション回数（とりあえず100〜500くらいでOK。論文用は1000推奨）
attr(sdm_result, "have_factor_preds") <- FALSE #エラー回避のためのおまじない（属性を追加）
imp_sdm <- impacts(
  sdm_result, 
  listw = listw_knn, 
  time  = 5,  # ※データの年数に合わせて変更してください（5年分なら5）
  R     = 1000
)

# 2. 結果の表示（z値やp値も見たい場合）
summary(imp_sdm, zstats = TRUE, short = TRUE)


## ===== train density plots ====
density_df <- read_excel("pref_density.xlsx")
library(fixest)
library(ggrepel)
library(ggplot2)
df <- read_excel("Final.xlsx")
df <- df %>%
  mutate(
    jp_school_density_area = jp_school_num / Inhabitable_area
  )
model_formula <- disappear_ratio ~ 
  foreigner_ratio + job_offer_ratio_avg + 
  share_primary + share_construction +
  share_manufacturing + min_salary + 
  jp_school_density_area + violation_count

#固定効果を抽出するために　lm_robust ではなく　feolsで
m2_for_extract <- feols(
  disappear_ratio ~ 
    foreigner_ratio + job_offer_ratio_avg +
    share_primary + share_construction +
    share_manufacturing + min_salary +
    jp_school_density_area + violation_count |
    Prefecture,
  data = df
)
# 2. 固定効果の値をデータフレームに取り出す
fixed_effects <- fixef(m2_for_extract)$Prefecture

df_fe <- data.frame(
  Prefecture = names(fixed_effects),
  FE_Value   = as.numeric(fixed_effects)
)
plot_data <- df_fe %>%
  inner_join(density_df, by = "Prefecture")



# 4. 散布図を描画
p <- ggplot(plot_data, aes(x = train_density, y = FE_Value, label = Prefecture)) +
  geom_point(color = "steelblue", alpha = 0.8, size = 2) +
  geom_smooth(method = "lm", color = "darkred", se = FALSE, linetype = "dashed") +
  geom_text_repel(size = 3, max.overlaps = 20) +
  labs(
    title    = "Railway Density vs Prefecture Fixed Effects",
    subtitle = "Testing 'Market Access' hypothesis",
    x        = "Railway Density (km/km²)",
    y        = "Prefecture Fixed Effect (Unobserved Disappearance Risk)"
  ) +
  theme_minimal()
print(p)
# 5. 相関係数と無相関検定（p値）を確認
cor_result <- cor.test(plot_data$density, plot_data$FE_Value)
print(cor_result)
context("Test palettes")

test_that("named_pal of diffrent classes", {
    col1 <- c("#00859b", "#00a78e", "#66cabb", "#b2dae1", "#563d82", "#7d3f98",
              "#b18cc1", "#b9afd6", "#aa0061", "#d20962", "#e46b95", "#e5b2cf")
    col2 <- c("#ff7f50", "#87cefa", "#da70d6", "#32cd32", "#6495ed", "#ff69b4")
    col3 <- c("#eff3ff", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")
    col4 <- c("#ff0000", "#ff4000", "#ff8000", "#ffbf00", "#ffff00", "#ffff80")
    col5 <- c("#b60a1c", "#e39802", "#309143", "#e03531", "#f0bd27", "#51b364")
    col6 <- c("#dc322f", "#b58900", "#d33682", "#268bd2", "#2aa198", "#859900")
    col7 <- c("#000004e5", "#3b0f70e5", "#8c2981e5", "#de4968e5", "#fe9f6de5",
                 "#fcfdbfe5")
    
    expect_equal(named_pal("aetnew_tea", 15, mode="no.fill"), col1)
    expect_message(named_pal("aetnew_tea", 15, mode="na.fill"), "NAs will")
    expect_equal(suppressWarnings(named_pal("aetnew_tea", 15, mode="na.fill")), c(col1, rep(NA, 3)))
    expect_message(named_pal("aetnew_tea", 15, mode="repeat.fill"), "will be repeated")
    expect_equal(suppressWarnings(named_pal("aetnew_tea", 15, mode="repeat.fill")), col1[c(1:12, 1:3)])
    expect_is(named_pal("aetnew_tea"), "function")
    expect_equal(named_pal("aetnew_tea")(10), col1[1:10])
    
    expect_equal(named_pal(NA, 6), col2)
    expect_equal(named_pal("blues", 6), col3)
    expect_equal(named_pal("heat", 6), col4)
    expect_equal(named_pal("tbl_traffic", 6), col5)
    expect_equal(named_pal("solarized_red", 6), col6)
    expect_equal(named_pal("magma", 6, alpha=0.9), col7)
    
    expect_error(suppressMessages(named_pal("not_available", 6)), "Invalid palette")
})

test_that("hex_colors of different classes", {
    expect_equal(hex_colors("red"), "#ff0000")
    expect_equal(hex_colors(2L), "#ff0000")
    expect_equal(hex_colors(c("red", "cyan")), c("#ff0000", "#00ffff"))
    expect_equal(hex_colors(c("red", "cyan"), unlist=FALSE), list("#ff0000", "#00ffff"))
    expect_equal(hex_colors("#444"), "#444444")
    expect_equal(hex_colors("aetnew_tea(2)"), c("#00859b", "#00a78e"))
    expect_equal(hex_colors("aetnew_tea", n=2), c("#00859b", "#00a78e"))
    expect_equal(hex_colors("aetnew_tea")(2), c("#00859b", "#00a78e"))
    
    expect_error(hex_colors("not_availble"), "can only be a valid argument for")
})


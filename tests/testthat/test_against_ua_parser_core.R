library(yaml)
library(testthat)

user.agent.tests <- c("firefox_user_agent_strings.yaml", "test_ua.yaml",
                      "pgts_browser_list.yaml",
                      "opera_mini_user_agent_strings.yaml")

for (user.agent.test in user.agent.tests) {
  testthat::context(user.agent.test)

  testthat::test_that(paste(user.agent.test, "is parsed correctly"), {
    raw_test_cases <- yaml::yaml.load_file(paste0("../test_resources/",
                                                  user.agent.test))

    test_cases <- do.call(rbind.data.frame, lapply(
      raw_test_cases$test_cases, function(x) {
        result <- data.frame(
          test = x$user_agent_string,
          browser = ifelse(is.null(x$family), "Other", x$family),
          browser_major = ifelse(is.null(x$major), "Other", x$major),
          browser_minor = ifelse(is.null(x$minor), "Other", x$minor),
          browser_patch = ifelse(is.null(x$patch), "Other", x$patch),
          browser_patch_minor = ifelse(is.null(x$patch_minor), "Other",
                                       x$patch_minor),
          stringsAsFactors = FALSE
        )
        result
      }))

    parsed <- uaparser::parse_agents(test_cases[["test"]])

    testthat::expect_equal(parsed[["browser"]], test_cases[["browser"]])
    testthat::expect_equal(parsed[["browser_major"]],
                           test_cases[["browser_major"]])
    testthat::expect_equal(parsed[["browser_minor"]],
                           test_cases[["browser_minor"]])
    testthat::expect_equal(parsed[["browser_patch"]],
                           test_cases[["browser_patch"]])
    testthat::expect_equal(parsed[["browser_patch_minor"]],
                           test_cases[["browser_patch_minor"]])
  })
}

os.tests <- c("test_os.yaml", "additional_os_tests.yaml")

for (os.test in os.tests) {
  testthat::context(os.test)

  testthat::test_that(paste(os.test, "is parsed correctly"), {
    raw_test_cases <- yaml::yaml.load_file(paste0("../test_resources/",
                                                  os.test))

    test_cases <- do.call(rbind.data.frame, lapply(
      raw_test_cases$test_cases, function(x) {
        result <- data.frame(
          test = x$user_agent_string,
          os = ifelse(is.null(x$family), "Other", x$family),
          os_major = ifelse(is.null(x$major), "Other", x$major),
          os_minor = ifelse(is.null(x$minor), "Other", x$minor),
          os_patch = ifelse(is.null(x$patch), "Other", x$patch),
          os_patch_minor = ifelse(is.null(x$patch_minor), "Other",
                                  x$patch_minor),
          stringsAsFactors = FALSE
        )
        result
      }))

    parsed <- uaparser::parse_agents(test_cases[["test"]])

    testthat::expect_equal(parsed[["os"]], test_cases[["os"]])
    testthat::expect_equal(parsed[["os_major"]], test_cases[["os_major"]])
    testthat::expect_equal(parsed[["os_minor"]], test_cases[["os_minor"]])
    testthat::expect_equal(parsed[["os_patch"]], test_cases[["os_patch"]])
    testthat::expect_equal(parsed[["os_patch_minor"]],
                           test_cases[["os_patch_minor"]])
  })
}

context("Gadget understocking component")

test_that("Can generate an understocking component with default parameters", {
    expect_equal(
        strsplit(as.character(gadget_likelihood_component("understocking")), "\n"),
        list(
            "[component]",
            "type\tunderstocking",
            "name\tunderstocking",
            "weight\t0"))
})

test_that("Can customise it", {
    expect_equal(
        strsplit(as.character(gadget_likelihood_component("understocking", name = "alfred", weight = 0.3)), "\n"),
        list(
            "[component]",
            "type\tunderstocking",
            "name\talfred",
            "weight\t0.3"))
})

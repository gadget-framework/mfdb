library(mfdb)
library(unittest, quietly = TRUE)
helpers <- c('utils/helpers.R', 'tests/utils/helpers.R') ; source(helpers[file.exists(helpers)])

mdb <- fake_mdb()

ok_group("mfdb_sample_totalweight", {
    ok(ut_cmp_error({
        mfdb_sample_totalweight(mdb, c(), list(), measurements = 'camel')
    }, "camel"), "Can't have unknown measurements, error included our mistake")
    ok(ut_cmp_error({
        mfdb_sample_totalweight(mdb, c(), list(), measurements = 'camel')
    }, "liver,"), "Can't have unknown measurements, error included correct columns")
})

ok_group("mfdb_sample_meanweight", {
    ok(ut_cmp_error({
        mfdb_sample_meanweight(mdb, c(), list(), measurements = 'camel')
    }, "camel"), "Can't have unknown measurements, error included our mistake")
    ok(ut_cmp_error({
        mfdb_sample_meanweight(mdb, c(), list(), measurements = 'camel')
    }, "liver,"), "Can't have unknown measurements, error included correct columns")
})

ok_group("mfdb_sample_meanweight_stddev", {
    ok(ut_cmp_error({
        mfdb_sample_meanweight_stddev(mdb, c(), list(), measurements = 'camel')
    }, "camel"), "Can't have unknown measurements, error included our mistake")
    ok(ut_cmp_error({
        mfdb_sample_meanweight_stddev(mdb, c(), list(), measurements = 'camel')
    }, "liver,"), "Can't have unknown measurements, error included correct columns")
})


# navigate to parent folder where the input file is saved
cd C:/repos/r61-r33-vccc-kumc/private/REDCap

# download latest docker img if not exists
docker run hello-world

# run the base degauss docker
# - `all`` argument will return all geocoded results regardless of precision
docker run --rm -v ${PWD}:/tmp ghcr.io/degauss-org/geocoder unique_addresses.csv all

# run other dockers for additional geomarkers
# - '2020' argument at the end will map to 2020 census block group
docker run --rm -v ${PWD}:/tmp ghcr.io/degauss-org/census_block_group:0.6.0 unique_addresses_geocoder_3.4.0_score_threshold_all.csv 2020

#!!! DON'T OPEN THE FILE IN EXCEL AS IT WILL CONVERT THE FORMAT
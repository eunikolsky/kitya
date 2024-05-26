#!/usr/bin/env bash

set -euo pipefail

auth="<paste auth here>"

OPTS=( --compressed -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:125.0) Gecko/20100101 Firefox/125.0' -H 'Accept: application/json' -H 'Accept-Language: en-US' -H 'DI-Backend: connectapi.garmin.com' -H "Authorization: $auth" )

while read id; do
  curl -f -o "$id.json" "https://connect.garmin.com/activity-service/activity/$id" "${OPTS[@]}"
  sleep 0.5
  curl -f -o "${id}_details.json" "https://connect.garmin.com/activity-service/activity/$id/details?maxPolylineSize=16384" "${OPTS[@]}"

  sleep 5
done <ids

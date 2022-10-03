#!/bin/bash
while read line; 
do
  c=$(echo "$line" | tr -d '\r')
  cmp "fetched/$c" "web_downloaded/$c";
done < discrepancies.txt

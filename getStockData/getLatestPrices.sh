#!/bin/bash

TICKER=$1
# 2020-03-01 00:00 UTC
#START=1583020800
#START=1420070400
START=1577836800
NOW=`date +%s`

curl "https://query1.finance.yahoo.com/v7/finance/download/$TICKER?period1=$START&period2=$NOW&interval=1d&events=history" > invest/prices/$TICKER.txt


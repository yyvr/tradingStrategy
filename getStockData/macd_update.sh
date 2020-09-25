#!/bin/bash

cd /Users/vir/Downloads
for t in `cat tickers.txt`
do
	./getLatestPrices.sh $t
done
sleep 1
# run R commandline
TS=`date`
echo "This is the report contents genereated at $TS" > report.out
cd invest/tradingStrategy
/usr/local/bin/Rscript findBuyAlert.R >> ../../report.out 
sleep 1

cd ..
cd ..
./sendreport.sh

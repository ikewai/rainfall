###BASH CODE TO DEFINE & WGET ALL FILES IF THEY EXIST 

#define TZ as HST
TZ=Pacific/Honolulu 

#define filedate year_month of yesterday
if [ $AGGREGATION_DATE]; then
    fileDate=$AGGREGATION_DATE
    fileYear=$(echo $AGGREGATION_DATE | cut -c1-4)
else
    fileDate=$(date --date="1 day ago" +"%Y_%m") 
    fileYear=$(date --date="1 day ago" +"%Y") 
fi
#define master dir
pathMaster=$'preliminary/rainfall/' 

#define url
urlMaster=$'https://ikeauth.its.hawaii.edu/files/v2/download/public/system/ikewai-annotated-data/HCDP/workflow_data/'

#define files
month_daily_rf=$pathMaster'data_outputs/tables/station_data/daily/partial_filled/statewide/Statewide_Partial_Filled_Daily_RF_mm_'$fileDate'.csv'
month_count_rf=$pathMaster'data_outputs/tables/rf_station_tracking/count/monthly/'$fileYear'_count_log_monthly_rf.csv'

#set dir
cd /home/hawaii_climate_products_container

#wget files and put in proper dir locations
wget $urlMaster$month_daily_rf -O './'$month_daily_rf || rm $month_daily_rf
wget $urlMaster$month_count_rf -O './'$month_count_rf || rm $month_count_rf


##PAU##

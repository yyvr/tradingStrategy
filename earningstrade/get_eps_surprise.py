'''
GET earnings surprise percentage
'''

import refinitiv.data as rd
import pandas as pd

def get_data():
    rd.open_session()

    '''
    # get nasdaq listed companies > 500m
    result = rd.discovery.search(
        view=rd.discovery.Views.EQUITY_QUOTES,
        top=1000,
        filter="(AssetState ne 'DC' and SearchAllCategoryv2 eq 'Equities' and (RCSExchangeCountry xeq 'G:6J' and MktCapCompanyUsd gt 500000000 and RCSAssetCategoryGenealogy eq 'A:1\A:1L' and ExchangeName xeq 'NASDAQ Global Select Consolidated'))",
        select="DTSubjectName,RIC,Gics,AssetState,MktCapCompanyUsd,TickerSymbol",
        order_by="RIC,MktCapCompanyUsd desc,DTSubjectName"
    )
    result.to_csv('searchResult.csv')    
    '''
    rd.close_session()

def get_predicted_surprises():
    rd.open_session()
    df = pd.read_csv('searchResult.csv', sep=',')
    rics = df.drop_duplicates(subset=['DTSubjectName'], keep='first')['RIC'].tolist()
    print(len(rics), ' stocks!!!')

    index = 892
    for ric in rics[892: ]:
        print(ric, index)
        index = index + 1
        surprise = rd.get_data(
            universe=[ric],
            fields=['TR.EpsPreSurprisePct(SDate=0,EDate=-3000,Period=FQ1,Frq=D).date',
                'TR.EpsPreSurprisePct(SDate=0,EDate=-3000,Period=FQ1,Frq=D)'])
        surprise.drop_duplicates(subset=['Date'], keep='first', inplace=True)
        surprise.rename(columns={'Earnings Per Share - Predicted Surprise PCT': 'predicted'}, inplace=True)
        surprise.dropna(subset=['Date', 'predicted'], inplace=True)
        surprise.to_csv('surprises/' + ric)

    rd.close_session()

def get_actual_surprises():
    # get actual earnings dates reported quarterly
    rd.open_session()

    df = pd.read_csv('searchResult.csv', sep=',')
    rics = df.drop_duplicates(subset=['DTSubjectName'], keep='first')['RIC'].tolist()
    print(len(rics), ' stocks!!!')

    report_dates = rd.get_data(
            universe=rics,
            fields=['TR.EPSActReportDate(SDate=0,EDate=-45,Period=FQ0,Frq=FQ)',
                    'TR.EPSActSurprise(SDate=0,EDate=-45,Period=FQ0,Frq=FQ)'])
    report_dates.rename(columns={'Earnings Per Share - Actual Surprise': 'actual_surprises'}, inplace=True)
    report_dates.dropna(subset=['Report Date', 'actual_surprises'], inplace=True)
    report_dates.to_csv('actual_earnings.csv', index=False)

    rd.close_session()

def compute_returns(prices, surprises, report_dates):
    results = pd.DataFrame(columns=['buyDate', 'sellDate', 'reportDate',
                                    'predicted_date', 'predicted', 'actual', 'ret'])

    for index, row in report_dates.iterrows():
        report_date = str(row['Report Date']).split(' ')[0]
        select = surprises.loc[surprises['Date'] == report_date, 'predicted']
        if select.empty:
            continue

        reported = surprises.loc[surprises['Date'] == report_date].index[0]
        if reported+1 >= len(surprises.index):
            continue

        surprise = surprises.predicted[reported + 1]
        found = prices.loc[prices['date'] == report_date, ]
        if found.empty:
            continue

        pi = found.index[0]
        tail = pi + 3
        if tail >= len(prices.index):
            tail = len(prices.index) - 1

        if pi - 1 < 0:
            pi = pi + 1

        ret = prices['close'].iloc[tail]/prices['close'].iloc[pi - 1] - 1

        new_row = pd.DataFrame({"buyDate": [prices['date'].iloc[pi - 1]],
                                "sellDate": [prices['date'].iloc[tail]],
                                'reportDate': prices['date'].iloc[pi],
                                'predicted_date': surprises.Date[reported + 1],
                                'predicted': surprise,
                                'actual': row['actual_surprises'],
                                'ret': ret})
        results = pd.concat([results, new_row])

    return results

def get_results():
    dates = pd.read_csv('actual_earnings.csv')

    df = pd.read_csv('searchResult.csv', sep=',')
    rics = df.drop_duplicates(subset=['DTSubjectName'], keep='first')

    all = pd.DataFrame()
    for index, row in rics.iterrows():
        print(index, row['RIC'])
        prices = pd.read_csv('/Users/yang/Downloads/invest/usprices/' + row['TickerSymbol'])
        surprises = pd.read_csv('surprises/' + row['RIC'])
        selected_dates = dates.loc[dates['Instrument'] == row['RIC'], ]
        results = compute_returns(prices, surprises, selected_dates)
        results['RIC'] = row['RIC']
        all = pd.concat([all, results])

    all.to_csv('all_results.csv')

if __name__=="__main__":
    #get_data()
    #get_actual_surprises()
    #get_predicted_surprises()
    get_results()


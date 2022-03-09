def selector(res, encode=None):
    from parsel import Selector
    if hasattr(res, 'ok'):
        if encode:
            resp = Selector(res.content.decode(encode))
        else:
            apparent_encoding = res.apparent_encoding
            if '2312' in apparent_encoding or 'dows' in apparent_encoding or '8859' in apparent_encoding:
                apparent_encoding = 'gbk'
            try:
                resp = Selector(res.content.decode(apparent_encoding))
            except:
                resp = Selector(res.content.decode(apparent_encoding, 'ignore'))
    else:
        if isinstance(res, str):
            resp = Selector(res)
        else:
            resp = Selector(res.text)
    return resp

def fetchData(year):
    import time
    import requests
    index = year
    for i in range(1, 1000):
        url = 'https://www.songfacts.com/browse/years/%s/page%s' % (index, i)
        headers = {
            'cookie': '_dlt=1; _dlt=1; _dlt=1; PHPSESSID=845a3b9a09d8bb3c422daca8cff0a1de; __uzma=614eb29e6cace6.15224289; __uzmb=1632547486; _gid=GA1.2.801041475.1632547487; __ssds=2; __ssuzjsr2=a9be0cd8e; __uzmaj2=2795a36b-047a-4930-b375-402a1e65e83f; __uzmbj2=1632547488; fsbotchecked=true; _fssid=e86c04d8-b323-4f89-a5f6-13d1df32112f; _dlt=1; _pbjs_userid_consent_data=3524755945110770; _pubcid=e5bae80b-1542-4369-bc83-07b112b9351d; __gads=ID=113ba8df0384cf05:T=1632557335:S=ALNI_MbTO-ZwM3zEdRvp4twaaXJ5oub5iA; _lr_env_src_ats=false; panoramaId_expiry=1632643742347; _cc_id=b15a6acef1961dd3f01095f1b45e75f0; cookie=%7B%22id%22%3A%2201FE1HK7D2XQGEWKZ5AXATK7F9%22%2C%22ts%22%3A1632557342679%7D; cto_bidid=6g3bEl9nUlk4Ukp5eGVUM3VQM1lNbEUlMkJFOHEwN1BOZ3RTN00zOVNZWlhiS24xdWJFWlRFMTZ1U2tEcmU4amlJRWMzbWI1amNtQ21taTJxQ2FGd1Nvem4lMkI2M01saEtUZTJ1RWZlbktQZUFPSDV3MEx5bGE1ZzZGdkZxVzd2SkVKUVg2UWNCYlolMkJ1MlZLdXQzNE03UXJOWHB3WGclM0QlM0Q; cto_bundle=PGOPd182QWdONXAxaWNaVngydVdxbk9pUzFMRU1FODBZREtpSDU1TU5KSmZSbHpscDY5alpxZDNxbDlad0wlMkIyRGFNY3pUVzdWbHFNRUJHUUhOcUtpMFElMkIlMkJGQ25PYnJwVFl6dlJEZHBLJTJGOW53dlhnJTJCTUVqeVZnS2JTVHFBVXB4OFpnWEpMVWNzVWViTVNGb2I4YTh4OFlpVGNadUtMTmRaYWN4VDIlMkJpQ0M1dHdqSEw2bzBrSzRHZjBra1RXbTRHNWhoemQ; __uzmc=1210212173702; __uzmd=1632576159; _gat=1; _ga_KV870MR8WK=GS1.1.1632576159.6.1.1632576160.0; _ga=GA1.1.506079265.1632547487; __uzmcj2=8712612172014; __uzmdj2=1632576161',

            'user-agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/94.0.4606.61 Safari/537.36'


        }
        poo = selector(requests.get(url, headers=headers, verify=False).text)

        data_list = poo.xpath('/html/body/div[6]/div/div/div/ul/li')
        print('Start scraping Page %s，total %s entries...'%(i,len(data_list)))
        if len(data_list) == 1: break
        for j in data_list:
            a1 = j.xpath('./a/text()').extract_first()
            a2 = j.xpath('./text()').extract_first()
            import csv
            out = open('%s.csv'%index, 'a', newline='', encoding='utf-8-sig')
            csv_write = csv.writer(out, dialect='excel')
            csv_write.writerow([a1,a2.replace('- ','')])
        time.sleep(3)


if __name__ == '__main__':
    for y in [str(i) for i in range(2000, 2022)]:
        fetchData(y)
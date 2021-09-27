import datetime
import json
import re
import sys
import pandas as pd

from bs4 import BeautifulSoup
import requests


class ParseException(Exception):
    pass

class Entry:
    '''Represent a single entry of the chart'''
    
    def __init__(self, title, artist, rank, peak, last, week, isNew):
        self.title = title
        self.artist = artist
        self.rank = rank
        self.peak = peak
        self.last = last
        self.week = week
        self.isNew = isNew

    def __str__(self):
        '''Return a string of 'TITLE by ARTIST'.'''
        if self.title:
            s = u"'%s' by %s" % (self.title, self.artist)
        else:
            s = u"%s" % self.artist

        if sys.version_info.major < 3:
            return s.encode(getattr(sys.stdout, "encoding", "") or "utf8")
        else:
            return s
    
    def json(self):
        '''Return the entry as a JSON string.'''
        return json.dumps(self, default=lambda o: o.__dict__, sort_keys=True, indent=4)

      
class Chart:
    '''Billboard Hot 100 chart at a specific date
    Attributes:
        date: the date of the chart
        entries: the list of Entry object ordered by rank'''
    
    def __init__(self, date=None, max_retry=5, timeout=25):
        '''Arguments:
                date: chart date, a string in YYYY-MM-DD format
                    If None, the lastest chart will be fetched.
                    Note that Billboard will return the chart published at the date nearest to the given date.
                    If it is not valid, no exception will be raised but a chart with no entries will be returned.
                max_retry: the max number of times to retry when requesting data
                    By default, max_retry is 5.
                timeout: the number of seconds to wait for a server response
                    If None, no timeout will be applied.'''
                    
        self.date = date
        self._max_retry = max_retry
        self._timeout = timeout
        self.entries = []
        self.fetchData()
        
    def fetchData(self):
        '''Get the chart data from Billboard'''
        
        def getEntry(entry_soup, selector):
            element = entry_soup.select_one(selector)
            if element:
                return element.text.strip()
            return None
    
        def getMeta(entry_soup, attr, ifNoValue=None):
            try:
                selected = entry_soup.select_one(
                    'span.chart-element__meta.text--%s' % attr
                )
                if (
                    not selected
                    or selected.string is None
                    or selected.string == "-"
                ):
                    return ifNoValue
                else:
                    return int(selected.string.strip())
            except:
                raise ParseException('Failed to parse metadata value: %s' % attr)
        
        if not self.date:
            url = 'https://www.billboard.com/charts/hot-100'
        else:
            url = 'https://www.billboard.com/charts/hot-100/' + self.date
        
        session = requests.Session()
        session.mount(
                'https://www.billboard.com',
                requests.adapters.HTTPAdapter(self._max_retry),
            )
        req = session.get(url, timeout=self._timeout)
        page_soup = BeautifulSoup(req.text, 'html.parser')
        
        dateElement = page_soup.select_one('button.date-selector__button.button--link')
        if dateElement:
            dateText = dateElement.text.strip()
            curDate = datetime.datetime.strptime(dateText, "%B %d, %Y")
            self.date = curDate.strftime("%Y-%m-%d")
        
        for entry_soup in page_soup.select('li.chart-list__element'):
            try:
                title = getEntry(entry_soup, 'span.chart-element__information__song')
            except:
                raise ParseException('Failed to parse title')
                
            try:
                artist = getEntry(entry_soup, 'span.chart-element__information__artist')
            except:
                raise ParseException('Failed to parse artist')
                
            try:
                rank = getEntry(entry_soup, 'span.chart-element__rank__number')
            except:
                raise ParseException('Failed to parse rank')
        
            peak = getMeta(entry_soup, 'peak')
            last = getMeta(entry_soup, 'last', ifNoValue=0)
            week = getMeta(entry_soup, 'week', ifNoValue=1)
            isNew = True if week == 1 else False
            
            entry = Entry(title, artist, rank, peak, last, week, isNew)
            self.entries.append(entry)
    
    def __getitem__(self, rank):
        '''Return the entry at a specific rank of the chart.'''
        return self.entries[rank-1]

    def __len__(self):
        '''Return the length of the chart.'''
        return len(self.entries)

    def json(self):
        '''Return the entry as a JSON string.'''
        return json.dumps(self, default=lambda o: o.__dict__, sort_keys=True, indent=4)

    def dataframe(self):
        '''Return the entry as a dataframe.'''
        df = pd.json_normalize(json.loads(self.json())['entries'])
        df['date'] = [self.date] * len(df)
        return df

    def csv(self, path=None):
        '''Output the chart to a csv file.'''
        if not path:
            path = self.date + '.csv'
        else:
            path += self.date + '.csv'
        if self.__len__() > 0:
            print('Outputting chart at %s ...' % self.date)
            return self.dataframe().to_csv(path, index=False)
        else:
            print('Failed to fetch chart at %s.' %self.date)

    def __str__(self):
        '''Return the chart as a multi-line string.'''
        if not self.date:
            s = 'chart (current) hot 100'
        else:
            s = "hot 100 chart from %s" % (self.date)
        s += "\n" + "-" * len(s)
        for n, entry in enumerate(self.entries):
            s += "\n%s. %s" % (entry.rank, str(entry))
        return s
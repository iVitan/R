# -*- coding:UTF-8 -*-
from threading import Thread
from queue import Queue
from datetime import datetime
from sqlalchemy import create_engine,String,Integer,DATE
from selenium import webdriver
from selenium.webdriver.common.desired_capabilities import DesiredCapabilities
import pandas as pd
import time

cities = ["上海"]
columns =['日期','AQI','质量等级','PM2.5','PM10','SO2','CO','NO2','O3_8h']

def prepare_month(start,end,fmt='%Y%m'):
    date_range = pd.date_range(start=start,end=end,freq='M')
    months = [datetime.strftime(date, fmt) for date in date_range]
    return months


def get_data(tasks):
    # For chrome
    chrome_options = webdriver.ChromeOptions()
    chrome_options.add_experimental_option("prefs",{"profile.managed_default_content_settings.images":2})
    dr = webdriver.Chrome(chrome_options=chrome_options)

    # # For phantomjs

    # dcap = dict(DesiredCapabilities.PHANTOMJS)
    # dcap['phantomjs.page.settings.loadImages']=False
    # dr=webdriver.PhantomJS(desired_capabilities=dcap)

    while not tasks.empty():
        city,month = tasks.get()
        dr.get('https://www.aqistudy.cn/historydata/daydata.php?city={0}&month={1}'.format(city, month))
        time.sleep(2)
        trs = []
        for tr in dr.find_elements_by_css_selector('tr')[1:]:
            tds = []
            for td in tr.find_elements_by_css_selector('td'):
                tds.append(td.text)
            trs.append(tds)
        df = pd.DataFrame(trs,columns=columns)
        df['city']=city
        df.to_csv("shanghaiWeather.csv",mode='a',header=False)
    dr.quit()

if __name__ == '__main__':
    threads=[]
    tasks=Queue()
    months=prepare_month(start='2014-01-01',end='2018-12-31')
    for city in cities:
        for month in months:
            tasks.put((city,month))
    for i in range(5):
        thread=Thread(target=get_data,args=(tasks,))
        threads.append(thread)
        thread.start()
    for thread in threads:
        thread.join()
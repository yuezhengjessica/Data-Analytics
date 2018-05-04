# -*- coding: utf-8 -*-
"""
Created on Wed Apr 25 10:56:04 2018

@author: yuezh
"""
import requests
# 东商城的信息
# 对数据保护好的网站 加kv  模拟网站

#访问不成功时候可以模拟浏览器 改user agent
#for example 访问amazon 网站被拒绝

#kv={'user-agent':'Mozilla/5.0'}
#r=requests.get(url,headers=kv)

def GetHtmlText(url):
    try:
        kv={'user-agent':'Mozilla/5.0'}
        r=requests.get(url,headers=kv)
        #查看属性
        r.raise_for_status()
        # 访问成功
        r.encoding=r.apparent_encoding
        return r.text[:1000]
    except:
        return 'Error'
    
url='https://primenow.amazon.com/dp/B000YD5OMM?qid=1524673438&m=A2VEV2SD9VM4EB&sr=1-2&ref_=pn_gw_tl_17739507011_2_img_A2VEV2SD9VM4EB&pf_rd_r=06B78WTD16WHKPK3XJB4&pf_rd_p=7bc217f3-02d3-4b3b-8ee5-7a1809d28f77&pf_rd_s=desktop-center-4&pf_rd_i=aa847cfac96c767feabd74c23d89df6ee0921c88&pf_rd_t=101&pf_rd_m=A35T2T8BW467U4'
GetHtmlText(url)



#2. 搜索引擎 
#   搜索关键词 加params
# kv 里面的q 或者wd 由网站决定
try:
    keyword='Python'
    kv={'q':keyword}
    r=requests.get('http://www.so.com/s',params=kv)
    print(r.request.url)
    #查看属性
    r.raise_for_status()
    # 访问成功
    r.encoding=r.apparent_encoding
    print(len(r.text))
except:
    print('Error')
    
    
# 网络图片的爬取和存储
    
url1='https://www.yelp.com/search?find_desc=Restaurants&find_loc=Civic+Center%2C+Manhattan%2C+NY&ns=1'
r1=requests.get(url1)
demo=r1.text

# 解析网页
from bs4 import BeautifulSoup
soup=BeautifulSoup(demo,'html.parser')
print(soup.prettify)
soup.a

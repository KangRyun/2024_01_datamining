from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options
import linkareer
import pandas as pd

url = "C:/Users/kr937/drive/2024/SS/data_mining/24_Data_mining/project/linkcareer_link.txt"

# Chrome 옵션과 서비스 설정
chrome_options = Options()
chrome_options.add_argument("--headless")  # 필요에 따라 Chrome 옵션 추가
service = Service("C:/Users/kr937/drive/2024/SS/data_mining/24_Data_mining/chromedriver-win64/chromedriver.exe")

# WebDriver 초기화
driver = webdriver.Chrome(service=service, options=chrome_options)

persons = [] 

# 작업에 WebDriver 사용
f = open(url, 'r')
while True:
    txt_link = f.readline()
    if txt_link == "":
        break
    person = linkareer.self_introduction(driver=driver, url=txt_link)
    persons.append(person)

df = pd.DataFrame(persons)
df.to_csv('링커리어_크롤링.csv', encoding='cp949')

driver.close()
f.close()
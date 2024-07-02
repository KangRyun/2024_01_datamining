from selenium import webdriver
from selenium.webdriver.common.by import By

def url_crawl(driver_path):
    # WebDriver 초기화
    driver = webdriver.Chrome(executable_path=driver_path)
    
    url_list = []
    with open("C:/Users/kr937/drive/2024/SS/data_mining/24_Data_mining/project/linkcareer_link.txt", 'w') as f:
        for page in range(1, 161):  # 페이지가 160페이지까지 존재함
            url = f"https://linkareer.com/cover-letter/search?keyword=데이터&page={page}&tab=all"
            driver.get(url)
            driver.implicitly_wait(3)
            
            # 페이지 내 특정 요소 찾기 (필요에 따라 조정)
            try:
                driver.find_element(By.XPATH, "/html/body/div[1]/div[1]/div/div[4]/div/div/section[1]/div[2]/div/div[3]/div[1]/div[1]/div[1]/a")
                # 링크 요소 찾기
                url_tags = driver.find_elements(By.TAG_NAME, 'a')
                for tag in url_tags:
                    url_name = tag.get_attribute('href')
                    if url_name and "cover-letter" in url_name and "search" not in url_name:
                        # 자소서 링크만 저장
                        print(url_name)
                        url_list.append(url_name)
            except Exception as e:
                print(f"Error on page {page}: {e}")
        
        driver.quit()
        
        # 중복 제거 후 파일에 쓰기
        for content in set(url_list):
            f.write(content + "\n")
            
def self_introduction(driver: webdriver.Chrome, url):
    person = {}
    driver.get(url)
    info = driver.find_element(By.XPATH, '//*[@id="__next"]/div[1]/div[4]/div/div[2]/div[1]/div[1]/div/div/div[2]/h1')
    specification = driver.find_element(By.XPATH, '//*[@id="__next"]/div[1]/div[4]/div/div[2]/div[1]/div[1]/div/div/div[3]/h3')
    content = driver.find_element(By.ID, "coverLetterContent")
    person['info'] = info.text  # 지원자 정보
    person['specification'] = specification.text  # 지원자 스펙
    person['self_intro'] = content.text  # 지원자 자소서
    print(person)
    return person
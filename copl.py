import json
import pickle
import re
import subprocess
import sys
import time

import urllib3
from bs4 import BeautifulSoup
from requests import Session
from urllib3.exceptions import InsecureRequestWarning


def main():
    session = CoPLSession(True)
    session.test(*map(int, sys.argv[1:]))


class CoPLSession(Session):
    INTERVAL = 2
    COOKIES_PICKLE = 'cookies.pickle'
    URL = 'https://www.fos.kuis.kyoto-u.ac.jp/~igarashi/CoPL/index.cgi'

    def __init__(self, login = False):
        super().__init__()
        self.timestamp = 0
        urllib3.disable_warnings(InsecureRequestWarning)
        if login:
            self.login()
        else:
            try:
                self.load_cookies()
            except:
                self.login()

    def __del__(self):
        self.save_cookies()
        print('saved')

    def login(self):
        with open('config.json') as f:
            data = json.load(f)
        data['command'] = 'login'
        super().post(CoPLSession.URL, data = data, verify = False)
        print('login')

    def load_cookies(self):
        with open(CoPLSession.COOKIES_PICKLE, 'rb') as f:
            self.cookies = pickle.load(f)

    def save_cookies(self):
        with open(CoPLSession.COOKIES_PICKLE, 'wb') as f:
            pickle.dump(self.cookies, f)

    def load_problem(self, num):
        self.wait()
        url = f'{CoPLSession.URL}?qno={num}'
        res = super().get(url, verify = False)
        self.soup = CoPLSession.create_soup(res)
        return res

    def post(self, derivation, game, num):
        self.wait()
        data = {
            'derivation': derivation,
            'command': 'answer',
            'game': game,
            'no': str(num)
        }
        url = f'{CoPLSession.URL}?qno={num}'
        res = super().post(url, data = data, verify = False)
        self.soup = CoPLSession.create_soup(res)
        return res

    def create_soup(res):
        res.encoding = res.apparent_encoding
        return BeautifulSoup(res.text, 'lxml')

    def wait(self):
        now = time.time()
        dt = now - self.timestamp
        t = max(0, CoPLSession.INTERVAL - dt)
        time.sleep(t)
        self.timestamp = now

    def test(self, first, last):
        for i in range(first, last + 1):
            self.load_problem(i)
            res = self.solve()
            if res.stderr:
                print(res.stderr)
                break
            else:
                self.post(res.stdout, self.game, i)
                if self.soup.h1.text[:2] == '正解':
                    print('\r', i, '/', last, sep = '', end = '')
                else:
                    print(i, ' incorrect')
                    break
        else:
            print('\rSolved:', first, '-', last)

    def solve(self):
        self.game = self.soup.find_all('p')[1].a.text
        system = re.sub('([A-Z]+)', r'_\1', self.game).lower()[1:]
        judgement = self.soup.select_one('pre[id=question]').text
        cmd = f'./_build/default/exec/{system}/main.exe'
        return subprocess.run(
            cmd, text = True, capture_output = True, input = judgement + ';;'
        )


if __name__ == '__main__':
    main()

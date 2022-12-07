import argparse
import os
import shlex
import subprocess
import tempfile

import requests
from bs4 import BeautifulSoup

def get_problem(year: str, day: str, session_file: str) -> str:
    url = f"https://adventofcode.com/{year}/day/{day.lstrip('0')}"
    session_token = open(session_file).read().strip()
    headers = {
        "cookie": f"session={session_token}"
    }
    return requests.get(url, headers=headers).text

def parse_body(body: str) -> str:
    return BeautifulSoup(body, "html.parser").find("main").prettify()

def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--day")
    parser.add_argument("--year")
    parser.add_argument("--session-file")
    return parser.parse_args()

def main():
    args = parse_args()
    with tempfile.NamedTemporaryFile() as f:
        f.write(parse_body(get_problem(args.year, args.day, args.session_file)).encode("utf-8"))
        subprocess.call(shlex.split(f"html-to-markdown {f.name}"))
        os.rename(f"dist/{os.path.basename(f.name)}.md", "problem.txt")
        os.rmdir("dist")

if __name__ == "__main__":
    main()

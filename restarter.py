import psutil
import urllib2
import datetime
import signal
import time

def killproc(procname):
    for proc in psutil.process_iter():
        # check whether the process name matches
        if procname in proc.name():            
            print(str(datetime.datetime.now())+" restarted")
            proc.kill()

def running(url, check):
    try:
        req = urllib2.Request(url, "")
        response = urllib2.urlopen(req)
        result = response.read()
        if result==check:
            return True
        else:
            print("not the expected response")
            return False
    except Exception, e:
        print("not there",e)
        return False
    return False

def timeout(signum, frame):
    print("timeout")
    raise Exception("end of time")
    
url = "http://127.0.0.1:8892/game?fn=ping"
check = """["hello"]"""

signal.signal(signal.SIGALRM, timeout)
signal.alarm(10)

try:
    if not running(url,check):
        print("killing")
        killproc("wasp-server")
except Exception, exc: 
    print exc


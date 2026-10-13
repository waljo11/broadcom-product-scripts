#
import os        #Import standard Python os lib
import platform  #Import standard Python platform lib
import sys       #Import standard Python sys lib
# Name the directory where the 'requests' are found
sys.path.append('/usslocation/forpython/python3.11/site-packages/pip/_vendor')
import requests
from requests.auth import HTTPBasicAuth
import json
#
def load_uss_credentials(file_path):
    """
    Reads a USS file containing credentials.
    Expected format:
    Line 1: User ID
    Line 2: Password
    """
    if not os.path.exists(file_path):
        print(f"CRITICAL ERROR: The USS file does not exist at: {file_path}")
        sys.exit(1)
    print(f"Reading credentials from USS file: {file_path}")
    try:
        # Open using standard UTF-8/ASCII configuration.
        # Python on z/OS automatically converts tagged EBCDIC files if needed.
        with open(file_path, 'r', encoding='utf-8', errors='replace') as f:   
            lines = [line.strip() for line in f.readlines() if line.strip()]  
        if len(lines) < 2:
            raise ValueError("The file must contain (User ID and Password).")
        userid = lines[0]
        password = lines[1]
        return userid, password
    except Exception as e:
        print(f"Error reading file {file_path}: {str(e)}")
        sys.exit(1)
#
# Replace with the actual absolute path to your USS file
USS_FILE_PATH = "/u/your/Secured/Credentials/location.txt"
# 1. Extract values
user, pwd = load_uss_credentials(USS_FILE_PATH)
print("Successfully loaded credentials from USS.")
# 2. Use them for your subsequent processing/API calls
# print(f"Target User ID: {user}")
# Proceed with your processing...

##  Build the complete url  here
print("#Arguments:", sys.argv[1:])   # Additional arguments passed
listticketnumber = sys.argv[1:2]
print("#listticketnumber :", type(listticketnumber), listticketnumber)
ticketnumber = listticketnumber[0]
                     
ticketnumberType = type(ticketnumber)
mySubstring = ticketnumber[0:3]               
#
url = "https://yoursite.service-now.com/api/now/table"
if mySubstring == 'PRB':
   print('problem ticket query')
   url = url + "/problem?sysparm_query=number="
else:
   print('Change ticket query')
   url = url + "/change_request?sysparm_query=number="
#
url = url + ticketnumber
print("#url:", url)                  # complete url
#
response = requests.get(url, auth=HTTPBasicAuth(user, pwd))
print(response.content)
out = json.loads(response.content)
n = out['result']
#
typeout = type(out)
typen   = type(n)
print("#n type", typen)
print("#out type", typeout)
#
if len(n) == 0:
   print("error - no output")
elif "number" in n[0].keys():
   print("Exists")
else:
   print("error - not found")
#   Caution: on USS:                         
#    Left  [ is a Hex AD     
#    Right ] is a Hex BD     
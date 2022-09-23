import json
import boto3
from boto3.session import Session
import json
import os



def download_previous_result(bucket, key, file_name):

    # Get s3 creds for SoS upload
    with open(os.path.join('mnt', 'data', 'input', 'output_conf.json')) as jsonfile:
        confluence_creds = json.load(jsonfile)


    session = Session(aws_access_key_id=confluence_creds['key'],
                    aws_secret_access_key=confluence_creds['secret'])
    s3 = session.client('s3')
    print(bucket, key, file_name)
    s3.download_file(bucket, key, file_name)
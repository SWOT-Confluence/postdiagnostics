import json
import boto3
import botocore
import json
import os
import sys

def download_previous_result(bucket, key, file_name):
    """Download previous priors results."""
    
    s3 = boto3.client("s3")
    try:
        response = s3.download_file(bucket, key, file_name)
        print(f"Downloaded: {file_name}.")
    except botocore.exceptions.ClientError as e:
        print(f"Could not download file: {file_name}.")
        print(f"Error - {e}")
        print("Cannot proceed without previous results. Exiting program.")
        sys.exit(1)
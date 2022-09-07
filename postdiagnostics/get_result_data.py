import boto3

def download_previous_result(bucket, key, file_name):
    s3 = boto3.client("s3")
    s3.download_file(bucket, key, file_name)
# Job Definition
resource "aws_batch_job_definition" "generate_batch_jd_postdiagnostics_flpe" {
  name                  = "${var.prefix}-postdiagnostics-flpe"
  type                  = "container"
  container_properties  = <<CONTAINER_PROPERTIES
  {
    "image": "${local.account_id}.dkr.ecr.us-west-2.amazonaws.com/postd-flpe",
    "executionRoleArn": "${data.aws_iam_role.exe_role.arn}",
    "jobRoleArn": "${data.aws_iam_role.job_role.arn}",
    "fargatePlatformConfiguration": { "platformVersion": "LATEST" },
    "logConfiguration": {
      "logDriver" : "awslogs",
      "options": {
        "awslogs-group" : "${data.aws_cloudwatch_log_group.cw_log_group.name}"
      }
    },
    "resourceRequirements": [
      {"type": "MEMORY", "value": "1024"},
      {"type": "VCPU", "value": "0.5"}
    ],
    "mountPoints": [
      {
        "sourceVolume": "input",
        "containerPath": "/mnt/data/input",
        "readOnly": true
      },
      {
        "sourceVolume": "flpe",
        "containerPath": "/mnt/data/flpe",
        "readOnly": true
      },
      {
        "sourceVolume": "diagnostics",
        "containerPath": "/mnt/data/output",
        "readOnly": false
      }
    ],
    "volumes": [
      {
        "name": "input",
        "efsVolumeConfiguration": {
          "fileSystemId": "${data.aws_efs_file_system.aws_efs_input.file_system_id}",
          "rootDirectory": "/"
        }
      },
      {
        "name": "flpe",
        "efsVolumeConfiguration": {
          "fileSystemId": "${data.aws_efs_file_system.aws_efs_flpe.file_system_id}",
          "rootDirectory": "/"
        }
      },
      {
        "name": "diagnostics",
        "efsVolumeConfiguration": {
          "fileSystemId": "${data.aws_efs_file_system.aws_efs_diagnostics.file_system_id}",
          "rootDirectory": "/postdiagnostics/reach"
        }
      }
    ]
  }
  CONTAINER_PROPERTIES
  platform_capabilities = ["FARGATE"]
  propagate_tags        = true
   retry_strategy {
    attempts = 3
  }
}

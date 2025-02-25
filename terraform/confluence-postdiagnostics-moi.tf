# Job Definition
resource "aws_batch_job_definition" "generate_batch_jd_postdiagnostics_moi" {
  name                  = "${var.prefix}-postdiagnostics-moi"
  type                  = "container"
  container_properties  = <<CONTAINER_PROPERTIES
  {
    "image": "${local.account_id}.dkr.ecr.us-west-2.amazonaws.com/${var.prefix}-postd-moi",
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
    "ephemeralStorage": {
      "sizeInGiB": 30
    },
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
        "sourceVolume": "moi",
        "containerPath": "/mnt/data/moi",
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
        "name": "moi",
        "efsVolumeConfiguration": {
          "fileSystemId": "${data.aws_efs_file_system.aws_efs_moi.file_system_id}",
          "rootDirectory": "/"
        }
      },
      {
        "name": "diagnostics",
        "efsVolumeConfiguration": {
          "fileSystemId": "${data.aws_efs_file_system.aws_efs_diagnostics.file_system_id}",
          "rootDirectory": "/postdiagnostics/basin"
        }
      }
    ]
  }
  CONTAINER_PROPERTIES
  platform_capabilities = ["FARGATE"]
  propagate_tags        = true
  tags                  = { "job_definition" : "${var.prefix}-postdiagnostics-moi" }
}

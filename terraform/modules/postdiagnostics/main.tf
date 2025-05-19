# Data sources
data "aws_caller_identity" "current" {}

resource "aws_cloudwatch_log_group" "cw_log_group" {
  name = "/aws/batch/job/${var.prefix}-postdiagnostics/"
}

# Local variables
locals {
  account_id = data.aws_caller_identity.current.account_id
}

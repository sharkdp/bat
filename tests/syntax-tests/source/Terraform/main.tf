provider "github" {
  organization = var.github_organization
}

resource "tls_private_key" "deploy_key" {
  algorithm = "RSA"
  rsa_bits  = "4096"
}

resource "null_resource" "private_key_file" {
  triggers = {
    deploy_key = tls_private_key.deploy_key.private_key_pem
  }

  provisioner "file" {
    content     = tls_private_key.deploy_key.private_key_pem
    destination = "~/${var.repo_name}_deploy_key.pem"

    connection {
      type        = "ssh"
      user        = "centos"
      private_key = var.terraform_ssh_key
      host        = var.server_ip
    }
  }

  provisioner "remote-exec" {
    inline = [
      "sudo mv ~/${var.repo_name}_deploy_key.pem /app/ssh_keys/",
      "sudo chmod 0400 /app/ssh_keys/${var.repo_name}_deploy_key.pem",
      "sudo chown app:app /app/ssh_keys/${var.repo_name}_deploy_key.pem",
    ]

    connection {
      type        = "ssh"
      user        = "centos"
      private_key = var.terraform_ssh_key
      host        = var.server_ip
    }
  }
}

resource "github_repository_deploy_key" "repo_deploy_key" {
  title      = "${var.env_name} Deploy Key"
  repository = var.repo_name
  key        = tls_private_key.deploy_key.public_key_openssh
  read_only  = var.read_only
}

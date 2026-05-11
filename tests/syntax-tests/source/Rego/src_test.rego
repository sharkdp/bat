package k8sazureprocmount

test_input_container_not_proc_mount_allowed {
    input := { "review": input_review, "parameters": input_parameters_default}
    results := violation with input as input
    count(results) == 0
}
test_input_container_proc_mount_not_allowed {
    input := { "review": input_review_unmasked, "parameters": input_parameters_default}
    results := violation with input as input
    count(results) == 1
}
test_input_container_proc_mount_not_allowed_null_param {
    input := { "review": input_review_unmasked, "parameters": null }
    results := violation with input as input
    count(results) == 1
}
test_input_container_proc_mount_not_allowed_missing_param {
    input := { "review": input_review_unmasked }
    results := violation with input as input
    count(results) == 1
}
test_input_container_many_not_proc_mount_allowed {
    input := { "review": input_review_many, "parameters": input_parameters_default}
    results := violation with input as input
    count(results) == 0
}
test_input_container_many_mixed_proc_mount_not_allowed {
    input := { "review": input_review_many_mixed, "parameters": input_parameters_default}
    results := violation with input as input
    count(results) == 1
}
test_input_container_many_mixed_proc_mount_not_allowed_two {
    input := { "review": input_review_many_mixed_two, "parameters": input_parameters_default}
    results := violation with input as input
    count(results) == 2
}
test_input_container_proc_mount_case_insensitive {
    input := { "review": input_review, "parameters": input_parameters_default_lower}
    results := violation with input as input
    count(results) == 0
}
test_input_container_not_proc_mount_unmasked {
    input := { "review": input_review, "parameters": input_parameters_unmasked}
    results := violation with input as input
    count(results) == 0
}
test_input_container_proc_mount_unmasked {
    input := { "review": input_review_unmasked, "parameters": input_parameters_unmasked}
    results := violation with input as input
    count(results) == 0
}
test_input_container_many_mixed_proc_mount_allowed_two {
    input := { "review": input_review_many_mixed_two, "parameters": input_parameters_unmasked}
    results := violation with input as input
    count(results) == 0
}

input_review = {
    "object": {
        "metadata": {
            "name": "nginx"
        },
        "spec": {
            "containers": input_containers_one
        }
    }
}

input_review_unmasked = {
    "object": {
        "metadata": {
            "name": "nginx"
        },
        "spec": {
            "containers": input_containers_one_unmasked
        }
    }
}

input_review_many = {
    "object": {
        "metadata": {
            "name": "nginx"
        },
        "spec": {
            "containers": input_containers_many,
            "initContainers": input_containers_one
        }
    }
}

input_review_many_mixed = {
    "object": {
        "metadata": {
            "name": "nginx"
        },
        "spec": {
            "containers": input_containers_many,
            "initContainers": input_containers_one_unmasked
        }
    }
}

input_review_many_mixed_two = {
    "object": {
        "metadata": {
            "name": "nginx"
        },
        "spec": {
            "containers": input_containers_many_mixed,
            "initContainers": input_containers_one_unmasked
        }
    }
}

input_containers_one = [
{
    "name": "nginx",
    "image": "nginx",
    "securityContext": {
      "procMount": "Default"
    }
}]

input_containers_one_unmasked = [
{
    "name": "nginx",
    "image": "nginx",
    "securityContext": {
      "procMount": "Unmasked"
    }
}]

input_containers_many = [
{
    "name": "nginx",
    "image": "nginx",
    "securityContext": {
      "procMount": "Default"
    }
},
{
    "name": "nginx1",
    "image": "nginx"
},
{
    "name": "nginx2",
    "image": "nginx",
    "securityContext": {
      "runAsUser": "1000"
    }
}]

input_containers_many_mixed = [
{
    "name": "nginx",
    "image": "nginx",
    "securityContext": {
      "procMount": "Default"
    }
},
{
    "name": "nginx1",
    "image": "nginx",
    "securityContext": {
      "procMount": "Unmasked"
    }
}]

input_parameters_default = {
     "procMount": "Default"
}

input_parameters_default_lower = {
     "procMount": "default"
}

input_parameters_unmasked = {
     "procMount": "Unmasked"
}

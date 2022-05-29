function tempdir()
    local stream = assert(io.popen('mktemp --directory'))
    local output = stream:read('*all')
    stream:close()
    return string.gsub(output, "\n", "")
end

function preprocess(path_or_url)
    filename_from_url = string.match(path_or_url, '^https?://.*/(.*)$')
    if filename_from_url then
        local temp_directory = tempdir()
        local new_path = temp_directory .. "/" .. filename_from_url

        -- TODO: how to prevent shell injection bugs?
        os.execute("curl --silent '" .. path_or_url .. "' --output '" .. new_path .. "'")

        return new_path
    else
        return path_or_url
    end
end

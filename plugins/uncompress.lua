function tempdir()
    local stream = assert(io.popen('mktemp --directory'))
    local output = stream:read('*all')
    stream:close()
    return string.gsub(output, "\n", "")
end

function preprocess(path)
    prefix = string.match(path, '^(.*)%.gz$')
    if prefix then
        local temp_directory = tempdir()
        local new_path = temp_directory .. "/" .. prefix

        -- TODO: how to prevent shell injection bugs?
        os.execute("gunzip < '" .. path .. "' > '" .. new_path .. "'")

        return new_path
    else
        return path
    end
end
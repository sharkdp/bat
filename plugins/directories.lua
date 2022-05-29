-- https://stackoverflow.com/a/3254007/704831
function is_dir(path)
    local f = io.open(path, "r")
    local ok, err, code = f:read(1)
    f:close()
    return code == 21
end

function preprocess(path)
    if is_dir(path) then
        tmpfile = os.tmpname()
        os.execute("ls -alh --color=always '" .. path .. "' > '" .. tmpfile .. "'")
        return tmpfile
    else
        return path
    end
end

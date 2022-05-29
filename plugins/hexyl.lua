-- Note: this plugin depends on the existence of 'inspect' [1] and 'hexyl' [2]
--
-- [1] https://github.com/sharkdp/content_inspector
-- [2] https://github.com/sharkdp/hexyl

function is_binary(path)
    local stream = assert(io.popen("inspect '" .. path .. "'"))
    local output = stream:read('*all')
    stream:close()
    return string.find(output, ": binary\n")
end

function preprocess(path)
    if is_binary(path) then
        tmpfile = os.tmpname()
        os.execute("hexyl --length 1024 --no-position --border=none --no-squeezing '" .. path .. "' > '" .. tmpfile .. "'")
        return tmpfile
    else
        return path
    end
end

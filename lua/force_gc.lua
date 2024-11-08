local function force_gc()
    collectgarbage("collect")
end

return force_gc

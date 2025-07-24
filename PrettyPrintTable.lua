function tableToPrettyString(tb)
  local convertToStr, getValueStr, getIndexStr
  
  local function indentMultiLineString(str, nestedCount)
    local indentedStr = str:gsub("[^\n]+", function(part)
      -- is part the start of the string? if not, then that means a newline char precedes it
      if str:find(part) ~= 1 then
        return string.rep(" ", nestedCount).. part
      end
    end)
    return indentedStr
  end
  
  function getIndexStr(index)
        local indexStr = tostring(index)
        return type(index) ~= "string" and string.format("[%s]", indexStr) or indexStr
  end
  function getValueStr(value, nestedCount)
    local valueType = type(value)
    if valueType == "table" then
      return convertToStr(value, nestedCount + 1)
    end
    if valueType == "number" then
      return tostring(value)
    end
    if valueType == "string" and value:find("\n") then
      return indentMultiLineString(value, nestedCount + 1)
    end
    
    return string.format("\"%s\"", tostring(value))
  end
  

    function convertToStr(tb, nestedCount)
        -- nestedCount offset of 1,because value inside tables are always indented (sorry i bad at explaining)
        local indentStr = string.rep(" ", nestedCount + 1)
        local str = "{\n"
        
        for index, value in pairs(tb) do
            local indexStr = getIndexStr(index)
            local valueStr = value ~= tb and getValueStr(value, nestedCount) or "*** CYCLE TABLE REFERENCE DETECTED ***"
            str = string.format(str.."%s%s = %s,\n", indentStr, indexStr, valueStr)
        end
        -- bracket indent str, no offset since it's not indented like values
        str = str.. string.rep(" ", nestedCount).. "}"
        return str
    end
  
    return convertToStr(tb, 0)
end

if(`test -n "-lxml2 -lz -llzma -licui18n -licuuc -licudata -lm -ldl"`) then

if(${?LD_LIBRARY_PATH}) then
    setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:-lxml2 -lz -llzma -licui18n -licuuc -licudata -lm -ldl
else
   setenv LD_LIBRARY_PATH -lxml2 -lz -llzma -licui18n -licuuc -licudata -lm -ldl
endif

endif
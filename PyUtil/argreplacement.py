import PyFort.fortExp as fe

# Replaces inline args with the given replacement args
# PARAMS:
# argReps -- the number of times to loop through looking at arguments (the
# number of arguments to look at); equal to the minimum of the number of
# inlineArgs and number of replacementArgs
# string -- the string in which arguments must be replaced
# inlineArgs -- arguments from the inline file (args to be replaced)
# replacementArgs -- arguments from the input file being processed
# RETURNS: a modified strings with all inlineArgs replaced by the 
# appropriate argument from replacementArgs
def replaceArgs(argReps,string,inlineArgs,replacementArgs):
    while argReps >= 0:
        string = replaceArg(string,\
                            str(inlineArgs[argReps]),\
                            str(replacementArgs[argReps]))
        argReps -= 1
    return string
    
# Replace every instance of one particular argument in a string
def replaceArg(string,inlineArg,replacementArg):
    strList = string.split(inlineArg)
    i = 1
    while i < len(strList):
        if (strList[i-1])[-1:].isalnum() or (strList[i])[:1].isalnum():
            strList[i-1] = strList[i-1]+inlineArg+strList[i]
            strList.pop(i)
        else:
            i += 1
    newStr = replacementArg.join(strList)
    return newStr

# Replaces inline args with the given args
# called on _son attributes
# PARAMS:
# arg -- the expression to be modified (is one of the sons of a statement)
# inlineArgs -- arguments from the inline file (args to be replaced)
# replacementArgs -- arguments from the input file being processed
# RETURNS: a modified expression to replace the old son in the statement
# being processed
def replaceSon(arg,inlineArgs,replacementArgs):
    newSon = arg
    if isinstance(arg,fe.Sel):
        try:
            index = inlineArgs.index(arg.head)
            head = replacementArgs[index]
            newSon = fe.Sel(head,arg.proj)
        except:
            pass
    elif isinstance(arg,fe.App):
        head = arg.head
        args = arg.args
        newArgs = []
        i = 0
        while i < len(arg.args):
            anArg = arg.args[i]
            if isinstance(anArg,fe.App) or isinstance(anArg,fe.Sel):
                newArgs.append(replaceSon(anArg,inlineArgs,replacementArgs))
            else:
                try:
                    index = inlineArgs.index(anArg)
                    newArg = replacementArgs[index]
                    newArgs.append(newArg)
                except:
                    newArgs.append(anArg)
            i += 1
        if len(newArgs) != 0:
            args = newArgs
        try:
            index = inlineArgs.index(arg.head)
            head = replacementArgs[index]
        except:
            pass
        newSon = fe.App(head,newArgs)
    else:
        try:
            index = inlineArgs.index(arg)
            newSon = replacementArgs[index]
        except:
            pass
    return newSon

from _Setup import *

from PyUtil.debugManager import DebugManager
import PyFort.fortStmts as fs
from PyFort.fortUnit import fortUnitIterator
import re,string
from PyFort.fortScan import scan1
from templateUnitMap import getTemplateUnit

# Handles template expansion
class TemplateExpansion(object):

    # set something here for the unit tests
    # set default template file here
    _templateFile = 'ad_template.f'

    @staticmethod
    def setTemplateFile(templateFile):
        TemplateExpansion._templateFile = templateFile

    def __init__(self,aUnit):
        self.__myUnit = aUnit
        self.__myNewDecls = []
        self.__myNewExecs = []
        # the current unit being inserted from the inline file (reverse mode)
        self.__inlineUnit = None
        

    # This function inserts the exec statements into self.__myNewExecs
    # in the order determined by the template file
    # PARAMS:
    # aUnit: a unit from the template file
    # Decls: Declaration statements from the original input file
    #  -- grouped by pragma number
    # (all decls in pragma 1 -- specified by Begin Replacement & 
    #  End Replacement comments --  in Decls[1])
    def __expandTemplateDecls(self,aUnit,Decls):
        for aDecl in aUnit.decls:
            if aDecl.is_comment():
                pat = re.compile(
                    "[ ]*[!][ ]*[$]template[_]pragma[_]declarations",
                    re.IGNORECASE)
                match = pat.search(aDecl.rawline)
                if match:
                    newStmt = fs.Comments(aDecl.rawline[:match.start()])
                    self.__myNewDecls.append(newStmt)
                    # return to input
                    if len(Decls) > 0:
                        for decl in Decls[0]:
                            if decl is not None and len(decl.rawline.strip()) != 0:
                                self.__myNewDecls.append(decl)
                        Decls[0] = None
                    # continue template
                    newStmt = fs.Comments(aDecl.rawline[match.end():])
                    self.__myNewDecls.append(newStmt)
                    continue
            self.__myNewDecls.append(aDecl)

        if Decls[0] != None:
            for decl in Decls[0]:
                if decl is not None and len(decl.rawline.strip()) != 0:
                    self.__myNewDecls.append(decl)            
        i = 1; j = 0
        while i < len(Decls):
            for aDecl in Decls[i]:
                if aDecl is not None and len(aDecl.rawline.strip()) != 0:
                    self.__myNewDecls.append(aDecl)
                j += 1
            j = 0
            i += 1        

    # This function inserts the exec statements into self.__myNewExecs
    # in the order determined by the template file
    # PARAMS:
    # aUnit: a unit from the template file
    # Execs: Exec statements from the original input file
    #  -- grouped by pragma number 
    # (all execs in pragma 1 -- specified by Begin Replacement & 
    #  End Replacement comments --  in Execs[1])
    def __expandTemplateExecs(self, aUnit, Execs):
        j = 0
        if len(Execs) > 0:
            for anInputExec in Execs[0]:
                if anInputExec is not None:
                    if anInputExec is not None and \
                            len(anInputExec.rawline.strip()) != 0:
                        self.__myNewExecs.append(anInputExec)
                j += 1
        execRepNum = 0
        firstIter = True
        for anExecStmt in aUnit.execs:
            if anExecStmt.is_comment():
                pat = re.compile(
                    "[!][ ]*[$]placeholder[_]pragma[$][ ]+id[=]",
                    re.IGNORECASE)
                match = pat.search(anExecStmt.rawline)
                if match:
                    newStmt = fs.Comments(anExecStmt.rawline[:match.start()])
                    self.__myNewExecs.append(newStmt)
                    endline = re.search('[\n]',anExecStmt.rawline[match.end():])
                    if endline:
                        end = match.end()+endline.start()
                        pragma = int(anExecStmt.rawline[match.end():end].strip())
                    else:
                        pragma = int(anExecStmt.rawline[match.end()].strip())
                    if pragma < len(Execs):
                        # return to input
                        for anInputExec in Execs[pragma]:
                            if anInputExec is not None and \
                                    len(anInputExec.rawline.strip()) != 0:
                                self.__myNewExecs.append(anInputExec)
                    # continue template
                    pat = re.compile("[0-9]+")
                    newmatch = pat.search(anExecStmt.rawline[match.end():])
                    newStmt = fs.Comments(anExecStmt.rawline[match.end()+newmatch.end():])
                    self.__myNewExecs.append(newStmt)
                continue
            anExecStmt = self.__insertSubroutineName(aUnit,anExecStmt)
            self.__myNewExecs.append(anExecStmt)


    # gets the name of the template used
    # RETURNS: the name of the template file (default, if none specified)
    def __getTemplateName(self):
        if self.__myUnit.cmnt is not None:
            template = \
                self.__getTemplateFromComment(self.__myUnit.cmnt)
            if template is not None:
                return template
        for aDecl in self.__myUnit.decls:
            if aDecl.is_comment():
                template = self.__getTemplateFromComment(aDecl)
                if template is not None:
                    return template
        for anExec in self.__myUnit.execs:
            if anExec.is_comment():
                template = self.__getTemplateFromComment(anExec)
                if template is not None:
                    return template
        for aStmt in self.__myUnit.end:
            if aStmt.is_comment():
                template = self.__getTemplateFromComment(aStmt)
                if template is not None:
                    return template
        return TemplateExpansion._templateFile #default template file

    # extracts the template name from a comment
    # PARAMS:
    # comment -- the input comment to be searched for a template name
    # RETURNS: the name specified by the comment or None
    def __getTemplateFromComment(self,comment):
        name = None
        p = re.compile('[$]openad[ ]+xxx[ ]+template[ ]+',re.IGNORECASE)
        match = p.search(comment.rawline)
        if match:
            end_name = re.search('[\n]|[ ]',comment.rawline[match.end():])
            if end_name:
                name = (comment.rawline[match.end():(match.end()+end_name.start())]).strip()
            else:
                name = (comment.rawline[match.end():]).strip()
        return name

    # replace '__SRNAME__' with the name of the subroutine being inlined
    # PARAMS:
    # Unit: the subroutine being processed (the one to replace __SRNAME__ with)
    # anExecStmt: an executive statement in which to replace all instances of
    # '__SRNAME__'
    def __insertSubroutineName(self,Unit,anExecStmt):
        match = re.search('__SRNAME__',anExecStmt.rawline,re.IGNORECASE)
        if match:
            plainLine=str(anExecStmt) # the line w/o continuation+linebreaks
            plMatch=re.search('__SRNAME__',plainLine,re.IGNORECASE)
            while plMatch:
                plainLine=plainLine[:plMatch.start()]+Unit.uinfo.name+plainLine[plMatch.end():]
                plMatch=re.search('__SRNAME__',plainLine,re.IGNORECASE)
            # redo scan and parse for the class of the given anExecStmt
            aNewExecStmt=anExecStmt.__class__.parse(scan1.scan(plainLine)[0],anExecStmt.lineNumber)
            aNewExecStmt.lead=anExecStmt.lead
            return aNewExecStmt.flow()
        return anExecStmt

    # Given a template file 'template' and the Decls and Execs from the file
    # being post-processed in reverse mode, insert all appropriate Decls, Execs, 
    # and inlined statements from template, inline, and original files in the unit
    # PARAMS:
    # Decls: a list of lists of processed declaration statements from the input
    # file, indexed by replacement pragma number
    # Execs: a list of lists of processed executive statements from the input
    # file, indexed by replacement pragma number
    # RETURNS: a processed unit after template expansion
    def expandTemplate(self,Decls,Execs):
        template = self.__getTemplateName()
        inputDeclNum = 0
        inputExecNum = 0
        pragma = 0
        aUnit=getTemplateUnit(template)    
        if aUnit.cmnt is not None:
            if self.__myUnit.cmnt is not None:
                self.__myUnit.cmnt.rawline = \
                    aUnit.cmnt.rawline+self.__myUnit.cmnt.rawline
            else:
                self.__myUnit.cmnt = aUnit.cmnt

        if isinstance(aUnit.uinfo,fs.SubroutineStmt):
            aUnit.uinfo.name = self.__myUnit.uinfo.name

        self.__expandTemplateDecls(aUnit, Decls)

        self.__expandTemplateExecs(aUnit, Execs)

        for endStmt in aUnit.end:
            newEndStmts = []
            if isinstance(endStmt,fs.EndStmt):
                match = re.search("template",endStmt.rawline,re.IGNORECASE)
                if match:
                    newEndStmt=fs.EndStmt(endStmt.lineNumber,endStmt.label,endStmt.lead)
                    newEndStmt.rawline=endStmt.rawline
                    newEndStmt.rawline = \
                        newEndStmt.rawline[:match.start(0)] + \
                        self.__myUnit.uinfo.name + \
                        newEndStmt.rawline[match.end(0):]
                    newEndStmts.append(newEndStmt)
                else: 
                    newEndStmts.append(endStmt)
        self.__myUnit.end = newEndStmts    
        self.__myUnit.decls = self.__myNewDecls
        self.__myUnit.execs = self.__myNewExecs
        return self.__myUnit

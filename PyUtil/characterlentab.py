from _Setup import *

import PyFort.fortExp as fe

class CharacterLenTab(object):
    def __init__(self):
        self.ids = dict() # string for the key and ArrayBoundsTabEntry for the value
        self.entry_counter=1

    def lookupCharLenId(self,charlenid):
        '''check for arrayid in the array bounds table'''
        if charlen_id in self.ids:
            return self.ids[charlen_id]
        else:
            return None

    def lookupCharLen(self,charLenExp):
        for entry in self.ids.values():
            if entry.charLenExp==charLenExp:
                return entry.charLenId
        return None

    def __enterNewCharLen(self,charLenExp):
        newEntry=CharacterLenTabEntry(charLenExp,self.entry_counter)
        self.ids[self.entry_counter]=newEntry
        self.entry_counter += 1
        return newEntry.charLenId

    def getCharLen(self,charLenExp):
        charLenId = self.lookupCharLen(charLenExp)
        if not charLenId:
            charLenId = self.__enterNewCharLen(charLenExp)
        return charLenId

class CharacterLenTabEntry(object):

    def __init__(self,charLenExp,charLenId):
        self.charLenExp=charLenExp    # expression for the character length
        self.charLenId=charLenId      # array bounds info id number in ArrayBoundsTab

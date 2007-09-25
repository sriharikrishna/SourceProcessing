import Setup

from unittest  import *
from freefmt   import *

class T1(TestCase):

    def test1(self):
        'simple free format string'

        ae = self.assertEquals
        a_ = self.assert_
        
        a_(True)

import os


class Parameters : 
    
    
    XML_FILE_NAME = 'input_file.xml'
    DEFAULT_XML_FILE_NAME = 'input_file.xml'
    XSD_FILE_NAME = 'input_file.xsd'

        
    def __init__(self):
        self.code_parameters = None # file name or string or... (?)
        self.default_parameters = None # file name or string or ... (?)
        self.schema = None # file name or string or ... (?)
        self._code_parameters_dir = os.path.dirname(os.path.realpath(__file__)) + '/../code_parameters/'

    def _read_file(self, file_name):
        file_path = self._code_parameters_dir + file_name
        file = open(file_path, 'r')
        file_str = file.read()
        file_str = str(file_str).encode('utf-8')
        file.close()
        return file_str


    def read(self):
        self.code_parameters = self._read_file(Parameters.XML_FILE_NAME)
        self.default_parameters = self._read_file(Parameters.DEFAULT_XML_FILE_NAME)
        self.schema = self._read_file(Parameters.XSD_FILE_NAME)

        
    def validate(self):
       #TBD
       pass
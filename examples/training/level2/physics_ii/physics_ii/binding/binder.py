import ctypes
import os
import logging
from pathlib import Path

import imas

from physics_ii.parameters import Parameters

from .data_type import IDSData
from .data_c_binding import ParametersCType


class PhysicsIIBinder:

    def __init__(self):
        pass
    
    def save_data(self, ids):
        pass
    
    def read_data(self, ids):
        pass
    
    
    def initialize(self):
        pass

    # only input arguments, outputs are returned (as a list if more than 1)
    def call_native_code(self, equilibrium0: imas.equilibrium, codeparams: Parameters) -> imas.equilibrium:
        """binding actor
        :param equilibrium0: equilibrium
        
        :param codeparams: code parameters, None implies default parameters
        :param result: equilibrium1,
        """
        #mds_dir = Path.home().joinpath('/public/')
        #mds_dir.mkdir( parents=True, exist_ok = True )

        db_entry = imas.DBEntry(imas.imasdef.MDSPLUS_BACKEND, 'tmp', 11, 22)
        db_entry.create()
        logger_physics_ii = logging.getLogger('binding')
        logger_physics_ii.setLevel(logging.ERROR)
    
        lib_location = os.path.dirname(os.path.realpath(__file__)) + '/../../wrapper/lib/libphysics_ii.so'
    
    
        _libactor_def = ctypes.CDLL( lib_location )
        _func_def = _libactor_def.physics_iiual

    
        # dict of input and output arguments
        arguments_dict = {}
        # their ordering
        arguments_order = []
        arguments_order_out = []
    
        # LOOP over arguments
        # ======   equilibrium0   ======

        arg = IDSData(db_entry, equilibrium0, 'in', 0).to_args()
    
        arguments_dict['equilibrium0'] = arg
        arguments_order.append('equilibrium0')
        
        # ======   equilibrium1   ======
        equilibrium1 = imas.equilibrium()
        arg = IDSData(db_entry, equilibrium1, 'out', 1).to_args()
    
    
        arguments_dict['equilibrium1'] = arg
        arguments_order.append('equilibrium1')
        arguments_order_out.append('equilibrium1')
        # end LOOP over arguments
    
        # check conflicting occurences and store data
        occ_dict = {}
        for arg in arguments_dict.values():
            if isinstance(arg['cval'], IDSData.IDSRef):
                occ_dict[arg['cval'].ids_name] = 1 + occ_dict.get(arg['cval'].ids_name, -1)
                arg['cval'].occurrence = occ_dict[arg['cval'].ids_name]
                # store input data
                if arg['in']:
                    db_entry.put(arg['value'], arg['cval'].occurrence)
        
    
    
        # XML Code Params 
        #  codeparams_str
        arg = {}
        arg['cval'] = ParametersCType(codeparams)
        arg['cref'] = ctypes.byref(arg['cval'])
        arg['in'] = True
        arg['out'] =  False
        arguments_dict['codeparams'] = arg
        arguments_order.append('codeparams')
        

        # End:  Code Params 
                
        # DIAGNOSTIC INFO
        # outputFlag
        arg = {}
        
        ptrOutputFlag = ctypes.c_int()
        arg["fc2k_array"] = False
        arg['cval'] = ptrOutputFlag 
        arg['cref'] = ctypes.byref(arg['cval'])
    
    
        arg['in'] = False
        arg['out'] = True
    
        arguments_dict['outputFlag'] = arg
        arguments_order.append('outputFlag')
    
        # diagnosticInfo_size -> auxiliary variable for C 
        arg = {}
        ptrDiagnosticInfo_size = ctypes.c_int()
        arg["fc2k_array"] = False
        arg['cval'] = ptrDiagnosticInfo_size
        arg['cref'] = ctypes.byref(arg['cval'])
    
    
        arg['in'] = False
        arg['out'] = True
    
        arguments_dict['diagnosticInfo_size'] = arg
        arguments_order.append('diagnosticInfo_size')
        
        # diagnosticInfo
        arg = {}
        
        ptrDiagnosticInfo = ctypes.c_char_p()
        arg["fc2k_array"] = True
        # we don't support unicode --> encode
        arg['cval'] = ptrDiagnosticInfo            # char*
        arg['cref'] = ctypes.byref(arg['cval'])    # char**
    
    
        arg['in'] = False
        arg['out'] = True
    
        arguments_dict['diagnosticInfo'] = arg
        arguments_order.append('diagnosticInfo')
    
        # end DIAGNOSTIC INFO
    
        # call the actor function
        arglist = [arguments_dict[k]['cref'] for k in arguments_order]

        _func = _func_def
        _func(*arglist)
    
    
        # Checking returned DIAGNOSTIC INFO
        diagnosticInfo_size = ptrDiagnosticInfo_size.value
        outputFlag = ptrOutputFlag.value
        diagnosticInfo = ptrDiagnosticInfo.value
    
        if diagnosticInfo_size < 1:
            diagnosticInfo = "<No diagnostic message>"
            
        if outputFlag < 0:
            raise Exception("Actor *** 'binding' *** returned an error (" + str(outputFlag) + "): '" + diagnosticInfo.decode('utf-8') + "'")
        if outputFlag > 0:
                logger_physics_ii.warning("Actor * 'binding' * returned diagnostic info: \n     Output flag:      ", outputFlag, "\n     Diagnostic info: ", diagnosticInfo.decode('utf-8'))
    
        # end DIAGNOSTIC INFO
    
        # get output data
        results = []
        for arg_name in arguments_order_out:
            arg = arguments_dict[arg_name]
            results.append(db_entry.get(arg['cval'].ids_name, arg['cval'].occurrence))
    

    
        # final output
        if not results:
            return None
        elif len(results) == 1:
            return results[0]
        else:
            return tuple(results)

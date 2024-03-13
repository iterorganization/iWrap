{% import './macros/%s_ids.jinja2' % code_description.implementation.data_type as ids_macro %}
{% import './macros/subroutines.jinja2' as sbrt_macro %}

{{ ids_macro.imports() }}
import  {{code_description.implementation.include_path | basename }};


class Wrapper4{{actor_description.actor_name}} {

    private {{code_description.implementation.include_path | suffix }} wrapped_code = new {{code_description.implementation.include_path | suffix }}();

    public Wrapper4{{actor_description.actor_name}} () {};

{% if code_description.implementation.subroutines.init.name %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                  WRAPPED INIT SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    {{ sbrt_macro.sbrt_definition("init", ids_macro, actor_description.actor_name, code_description.implementation.subroutines.init,
    code_description.implementation.code_parameters.parameters) }}
{% endif %}

{% if code_description.implementation.subroutines.finalize.name %}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                   WRAPPED FINALIZE SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    {{ sbrt_macro.sbrt_definition("finalize", ids_macro, actor_description.actor_name, code_description.implementation.subroutines.finalize,
    code_description.implementation.code_parameters.parameters) }}
{% endif %}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//                                   WRAPPED MAIN SBRT CALL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{{ sbrt_macro.sbrt_definition("main", ids_macro, actor_description.actor_name, code_description.implementation.subroutines.main,
code_description.implementation.code_parameters.parameters) }}


{% if code_description.implementation.subroutines.get_state %}
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    //                                  WRAPPED GET STATUS SBRT CALL
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    public String call_get_state() throws Exception
    {
        String state_str = "";

        // - - - - - - - - - - - - - WRAPPED CODE CALL - - - - - -- - - - - - - - - - - -
        state_str = this.wrapped_code.{{code_description.implementation.subroutines.get_state}}();
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        return state_str;
    }
{% endif %}

{% if code_description.implementation.subroutines.set_state %}
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    //                                  WRAPPED SET STATUS SBRT CALL
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    public void call_set_state(String state_str) throws Exception
    {
        // - - - - - - - - - - - - - WRAPPED CODE CALL - - - - - -- - - - - - - - - - - -
        this.wrapped_code.{{code_description.implementation.subroutines.set_state}}(state_str);
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    }
{% endif %}

{% if code_description.implementation.subroutines.get_timestamp %}
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    //                                  WRAPPED GET STATUS SBRT CALL
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    public double call_get_timestamp() throws Exception
    {
        double timestamp = -1;

        // - - - - - - - - - - - - - WRAPPED CODE CALL - - - - - -- - - - - - - - - - - -
        timestamp = this.wrapped_code.{{code_description.implementation.subroutines.get_timestamp}}();
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        return timestamp;
    }
{% endif %}
}

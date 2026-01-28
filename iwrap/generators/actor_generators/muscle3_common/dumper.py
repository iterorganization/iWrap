import yaml


class CustomDumper(yaml.SafeDumper):
    def ignore_aliases(self, data):
        return True

    #dump empty sequence as: ' ' (nothing)
    def represent_sequence(self, tag, sequence, flow_style=None):
        if not sequence:  # Check if the sequence is empty
            return super().represent_scalar(u'tag:yaml.org,2002:null', u'')
        return super().represent_sequence(tag, sequence, flow_style)

    # dump empty dict as: ' ' (nothing)
    def represent_mapping(self, tag, mapping, flow_style=None):
        if not mapping:  # Check if the sequence is empty
            return super().represent_scalar(u'tag:yaml.org,2002:null', u'')
        return super().represent_mapping(tag, mapping, flow_style)

    def represent_none(self, data):
        return super().represent_scalar('tag:yaml.org,2002:null', u'')

    # dump empty dict as: ' ' (nothing)
    def represent_scalar(self, tag, value, style=None):
        if not value or value.lower() == 'null':  # Check if value is None or 'null'
            return super().represent_scalar(u'tag:yaml.org,2002:null', u'')
        return super().represent_scalar(tag, value, style)

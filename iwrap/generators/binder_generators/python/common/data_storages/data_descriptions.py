class IDSDescription:
    """Identifies a single IDS inside an IMAS data entry using URI.
    """

    def __init__(self, base_uri: str, ids_name: str, occurrence: int):
        self.base_uri = base_uri
        self.ids_type = ids_name
        self.occurrence = occurrence
        self.intent = None

    @property
    def uri(self) -> str:
        """IMAS URI including IDS type and occurrence fragment."""
        return f"{self.base_uri}#{self.ids_type}:{self.occurrence}"

    def save(self, stream):
        stream.write("------- IDS -------\n")
        stream.write(self.ids_type)
        stream.write("\n")
        stream.write(str(self.occurrence))
        stream.write("\n")
        stream.write(self.base_uri)
        stream.write("\n")

    def convert_to_native_type(self):
        pass

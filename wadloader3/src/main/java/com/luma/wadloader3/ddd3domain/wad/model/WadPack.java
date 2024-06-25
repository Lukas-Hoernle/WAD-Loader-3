package com.luma.wadloader3.ddd3domain.wad.model;

import jakarta.persistence.*;
import lombok.*;

import java.util.Map;

@Entity
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "WadPacks")
public class WadPack {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private int id;

    @Column(name = "name")
    private String name;

    @Column(name = "description")
    private String description;

    @ManyToMany(cascade = {CascadeType.DETACH, CascadeType.MERGE, CascadeType.PERSIST, CascadeType.REFRESH})
    @JoinTable(joinColumns = {@JoinColumn(name = "wad_pack_id", referencedColumnName = "id")},
            inverseJoinColumns = {@JoinColumn(name = "wad_id", referencedColumnName = "id")})
    @MapKeyColumn(name = "wad_order")
    private Map<Integer, Wad> wads = Map.of();
}

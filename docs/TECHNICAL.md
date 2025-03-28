Technical Specification
Introducción y Visión General
El protocolo implementa una plataforma descentralizada de crowdfunding basada en la blockchain de Cardano, combinando la seguridad y transparencia de los smart contracts con la usabilidad dei una aplicación web moderna. El sistema está diseñado para permitir que proyectos recauden fondos de manera segura y transparente, mientras protege los intereses de los inversores mediante un sistema de hitos verificables.
Conceptos Clave
Protocolo
El término "Protocolo" abarca la totalidad de nuestro producto, incluyendo los smart contracts desplegados en blockchain, la aplicación web que sirve como interfaz de usuario, la base de datos que almacena información extendida, los sistemas de sincronización entre capas y todos los servicios auxiliares necesarios para su funcionamiento.
Campaña
Representa un proyecto de crowdfunding que existe simultáneamente en dos capas:
En la blockchain, mantiene los elementos críticos para su operación: objetivos de recaudación, estados actuales, configuración de milestones, registros de transacciones y lista de billeteras autorizadas para su gestión. Esta información se almacena en los datums de los contratos y es inmutable.
En la base de datos, almacena toda la información descriptiva y complementaria: descripción detallada del proyecto, roadmap completo, perfiles del equipo, materiales multimedia e historial de actualizaciones. También guarda registros de interacciones como comentarios y discusiones.
Blockchain, Red u On-Chain
Es el entorno donde residen y operan los smart contracts. Aquí se almacenan los datums y se ejecutan todas las transacciones de manera inmutable y verificable. Cuando nos referimos a operaciones "en la red" o "on-chain", hablamos de interacciones que ocurren directamente en la blockchain de Cardano.
Off-Chain
Comprende todos los componentes que operan fuera de la blockchain, concretamente nuestra aplicación web y la base de datos. Este entorno maneja la interfaz de usuario, almacena información descriptiva y gestiona interacciones que no requieren la inmutabilidad de la blockchain.
Smart Contract / Script
Un smart contract en Cardano (también llamado script) es un programa que se ejecuta en la blockchain y define reglas específicas para la gestión de activos digitales. En nuestro protocolo, utilizamos tres tipos de contratos principales: Protocolo, Campaña y Fondos. Cada uno tiene su propia lógica y reglas de validación que determinan cómo y cuándo se pueden realizar diferentes operaciones.
Datum
Es el estado almacenado en la blockchain que contiene la información crítica necesaria para el funcionamiento de los smart contracts. Cada contrato (Protocolo, Campaña, Fondos) tiene su propio datum con información específica y permisos de operación.
UTXO (Unspent Transaction Output)
Los UTXOs son el modelo que utiliza Cardano para representar y su ledger interno. Las UTXOs funcionan como "cajas" que contienen:
Activos (ADA y tokens)
Un datum (estructura de datos de cualquier tipo)
Una dirección de script que lo controla
Cada vez que se realiza una operación, se consumen UTXOs existentes y se crean nuevos. En nuestro protocolo, cada campaña y sus fondos están representados por diferentes UTXOs que mantienen su respectivo datum y activos.
Transacciones
En la Red (On-Chain)
Las transacciones en la blockchain son operaciones atómicas que:
Consumen uno o más UTXOs existentes
Crean nuevos UTXOs
Pueden acuñar o quemar tokens
Actualizan datums
Requieren validación por los smart contracts
Son inmutables una vez confirmadas
Tienen un costo en ADA (fee)
Por ejemplo, cuando un usuario compra tokens de una campaña, la transacción:
Consume el UTXO de fondos actual
Crea un nuevo UTXO con el estado del datum actualizado
Transfiere ADA del contrato de fondos de la campaña
Entrega tokens al comprador
En la Base de Datos (Off-Chain)
Las transacciones en la base de datos son registros que:
Reflejan las operaciones realizadas en la blockchain
Facilitan búsquedas y filtrados
Ayudan a mostrar historiales de operaciones
No tienen costo operativo
Pueden ser revertidas si es necesario 
La sincronización entre ambos sistemas asegura que la información en la base de datos refleje fielmente el estado real en la blockchain, mientras permite mantener datos adicionales necesarios para la operación del marketplace.
Entity
Las entidades representan la estructura fundamental de datos en nuestro sistema, siguiendo el patrón de Domain Driven Design. En nuestra arquitectura existen dos tipos principales de entidades:
Regular Entity
Representa datos tradicionales que solo necesitan persistencia en base de datos
Mapea directamente a una tabla en PostgreSQL
Sigue el patrón tradicional de ORM con TypeORM
SmartDB Entity
Extiende el concepto de entidad regular para integrar datos que viven tanto en la base de datos como en la blockchain
Permite sincronización automática entre el estado off-chain y on-chain
Gestiona la consistencia entre ambos sistemas
Para una explicación detallada de cómo el framework SmartDB implementa y gestiona estas entidades, incluyendo su sincronización, validación y características especiales, ver la sección "Framework SmartDB".
Las entities forman la columna vertebral de nuestro sistema de datos, proporcionando una abstracción que permite trabajar con información tanto en blockchain como en base de datos de manera unificada y transparente.
Usuarios del Sistema
Wallet (Billetera)
En nuestro sistema, una wallet de Cardano representa la identidad principal de un usuario. La conexión de una wallet es el primer paso para interactuar con el protocolo, funcionando como un sistema de autenticación nativo de Web3. Cada wallet está identificada por su payment public key hash, que actúa como un identificador único e inmutable.
Los usuarios pueden posteriormente vincular información adicional a su wallet, como email, perfil y preferencias, enriqueciendo así su identidad en el sistema mientras mantienen la wallet como punto central de autenticación y autorización para operaciones blockchain.
La entidad principal en nuestra base de datos se denomina wallet ("Billetera") y representa esta dualidad: la identidad blockchain (wallet) y la información off-chain asociada (perfil de usuario).
Tipos de Usuarios
Protocol Team
Son los administradores del protocolo, cuyas billeteras están registradas en el Datum del Protocolo. Tienen la capacidad de aprobar campañas, validar hitos y ejecutar operaciones administrativas tanto en la blockchain como en la base de datos.
AKA: Team, Equipo o simplemente Protocolo
Campaign Managers
Son las billeteras registradas en el Datum de cada Campaña específica. Pueden ejecutar transacciones en la blockchain relacionadas con su campaña, como solicitar validación de hitos y gestionar fondos liberados. También tienen acceso completo a la gestión de su campaña en la base de datos.
AKA: Managers o Administradores
Campaign Editors
Son usuarios identificados por su email que tienen permisos para gestionar el contenido de una campaña específica en la base de datos. No pueden ejecutar transacciones en la blockchain, pero pueden actualizar información, gestionar materiales multimedia y mantener la comunicación con la comunidad.
AKA: Editors
Investors
Son la gran mayoría de los usuarios del protocolo con cualquier wallet conectada. Son aquellos que van a participar de alguna campaña aportando fondos. Pueden ejecutar las transacciones de compra de tokens.
AKA: Users
Capas del Sistema
Blockchain (On-Chain)
Es la capa donde residen y se ejecutan los smart contracts, almacenando sus datums y registrando todas las transacciones de forma inmutable. En nuestro contexto, cuando hablamos de "la red" o "on-chain" nos referimos a esta capa blockchain. Aquí es donde:
Se despliegan y ejecutan los smart contracts
Se almacenan los diferentes datums
Se procesan y validan todas las transacciones
Se gestionan los fondos y tokens
Componentes On-Chain
El protocolo se implementa mediante tres contratos inteligentes principales que interactúan entre sí para gestionar todo el ciclo de vida de las campañas:
Contrato de Protocolo: Actúa como núcleo del sistema, almacenando la lista de administradores autorizados y controlando la creación y validación de campañas. Este contrato es el punto central de la administración y establece las reglas fundamentales que rigen todas las operaciones.
Contrato de Campaña: Representa cada campaña individual de crowdfunding, almacenando sus parámetros esenciales, estado actual y lógica de operación. Gestiona los objetivos de recaudación, hitos y estados de la campaña.
Contrato de Fondos: Maneja la gestión real de los fondos para cada campaña, procesando depósitos, retiros y la liberación gradual de fondos según el cumplimiento de hitos.
Off-Chain
El sistema off-chain comprende varios componentes que trabajan en conjunto:
Website (Frontend)
Una aplicación web moderna que proporciona la interfaz de usuario del protocolo:
Home: Escaparate principal de campañas destacadas
More Campaigns: Lista completa de campañas para invertir
Drafts & Manage: Panel de gestión para creadores y administradores
Campaign Details: Páginas detalladas de cada campaña
Campaign Creation: Interfaz guiada para crear nuevas campañas
Campaign Management: Página con los detalles de la campaña y botones para editar los diferentes campos
Wallet Connector: Componente para conectar billeteras Cardano
User Dashboard: Gestión de perfil y actividades del usuario
Ver más de la interfaz de usuario
Website (Backend)
Implementado sobre el framework SmartDB proporciona:
API handlers automáticos para cada entity
Gestión de autenticación y autorización
Construcción y gestión de transacciones blockchain
Sincronización automática entre blockchain y base de datos
Website (Admin)
Panel administrativo para gestionar:
Tablas internas del sistema
Configuraciones de SmartDB
Monitoreo de sincronización
Gestión de entities
Base de Datos (PostgreSQL)
La plataforma mantiene una base de datos tradicional que complementa la información almacenada en la blockchain. Esta base de datos contiene elementos descriptivos extensos que no requieren la inmutabilidad de la blockchain, como descripciones detalladas, imágenes, actualizaciones y comentarios.
La base de datos almacena:
Entities regulares y SmartDB entities
Tablas del sistema SmartDB:
address_to_follow: Mapeo de direcciones y tipos de datums
smart_utxo: Control de UTXOs del sistema
site: Configuraciones globales
job: Control de procesos en ejecución
token_metadata: Cache de metadata de tokens
transactions: Historial de transacciones realizadas
wallet: billeteras de usuarios
Framework SmartDB
SmartDB es un framework integral diseñado para simplificar el desarrollo de aplicaciones descentralizadas (dApps) en Cardano, proporcionando una capa de abstracción que conecta de manera transparente los sistemas off-chain con la blockchain.
Componentes Principales
Entity Management System
El sistema de entities es el núcleo del framework, proporcionando una abstracción similar a ORM para la gestión de datos:
Regular Entities
Mapean directamente a tablas en la base de datos
Similares a modelos tradicionales de TypeORM
Gestionan datos puramente off-chain
SmartDB Entities
Extienden las regular entities con capacidades blockchain
Sincronizan campos específicos con datums on-chain
Mantienen consistencia entre estado blockchain y base de datos
Sistema de Sincronización
La sincronización automática entre blockchain y base de datos se maneja a través de varios componentes especializados:
Blockchain Scanner
Monitorea continuamente addresses registradas
Procesa nuevos transacciones en tiempo real
Interpreta datums para actualización de entities
Database Synchronizer
Ejecuta actualizaciones en batch para optimizar performance
Implementa resolución de conflictos
Transaction Management
Sistema robusto para la gestión de transacciones blockchain que maneja el flujo completo desde la creación hasta la confirmación:
Transaction Builder
Construye transacciones Cardano utilizando Lucid
Maneja smart contract interactions y validación de constraints
Implementa retry policies para casos de fallo
Flujo de Transacciones
Iniciación
Frontend solicita construcción de tx con parámetros específicos
Backend valida permisos y parámetros
Construcción
Backend prepara la transacción
Selecciona UTXOs necesarios
Construye tx usando Lucid
Adjunta scripts y validators requeridos
Devuelve la tx serialized a al Frontend 
Procesamiento
Frontend obtiene firma del usuario vía wallet y envía la tx firmada al Backend nuevamente
Backend envía tx a la red y crea entrada en nuestra base de datos de esa tx para darle seguimiento
Sistema monitorea confirmación
Finalización
El Backend actualiza estado en base de datos
Notifica resultado a Frontend
Libera recursos reservados
UTXO Management
Smart Selection Algorithm para optimizar selección de UTXOs
Sistema de locking para prevenir double-spending
Gestión de concurrencia
API System
Genera y gestiona automáticamente APIs para cada entity:
API Generation
Endpoints RESTful automáticos
Documentación OpenAPI
Validación de requests
Security Layer
Autenticación via JWT
Control de acceso basado en roles
Herramientas para Frontend
El framework proporciona utilities específicas para frontend:
React Hooks
useWalletActions: Gestión de conexión wallet
Store Management
App Store: Estado global
Wallet Store: Estado de conexión
Características Adicionales
Performance
Query optimization
Connection pooling
Caching inteligente
Developer Experience
CLI tools para generación de código
Security
Input sanitization
CORS management
SQL injection prevention
SmartDB simplifica significativamente el desarrollo de dApps en Cardano, proporcionando una infraestructura robusta y herramientas que permiten a los desarrolladores focalizarse en la lógica de negocio mientras el framework maneja la complejidad de la integración blockchain.
Sistema de Identificación y Acceso
Billeteras y Wallet Connector
El sistema utiliza las billeteras de Cardano como método principal de identificación y autenticación. El Wallet Connector permite a los usuarios:
Conectar cualquier billetera compatible con Cardano
Vincular un email a su billetera para funcionalidades adicionales
Firmar transacciones de forma segura
Verificar su identidad para diferentes operaciones
Vinculación de Email
El sistema permite vincular direcciones de email a las billeteras por dos razones principales:
Facilitar la comunicación y notificaciones
Habilitar permisos específicos para roles de edición
Jerarquía de Permisos
El sistema implementa una estructura jerárquica de permisos basada en la combinación de billeteras y emails:
Protocol Team
El equipo central del protocolo, cuyas billeteras están registradas en el datum del protocolo, tiene la capacidad de:
Máximo nivel de privilegios
Aprobar nuevas campañas
Validar cumplimiento de hitos
Gestionar parámetros globales
Ejecutar operaciones de emergencia
Campaign Managers
Billeteras registradas en el Datum de cada Campaña
Gestión completa de su campaña específica
Capacidad de ejecutar transacciones on-chain
Solicitar validación de hitos
Administrar fondos liberados
Campaign Editors
Identificados por email vinculado
Permisos limitados a la base de datos
Sin capacidad de ejecutar transacciones blockchain
Pueden actualizar contenido y metadata de la campaña
Publicar actualizaciones
Responder comentarios
Investors
Capacidad de ejecutar transacciones de compra de tokens on-chain
Interfaz de Usuario
La plataforma ofrece una interfaz web intuitiva con las siguientes secciones principales:
Home (/index)
Presenta un listado curado de campañas, permitiendo a los usuarios explorar y descubrir proyectos interesantes. La página incluye filtros y sistemas de búsqueda para facilitar la navegación. Se muestran las campañas marcadas como featured.
Ver estados en website y más detalles del home
More Campaigns (/campaings)
Presenta el listado completo de campañas con las que el usuario inversor puede interactuar. Es similar en estructura y botones que el Home. 
Drafts & Manage (/manage)
Una sección especializada donde los creadores de campañas y administradores pueden gestionar sus proyectos. Aquí se visualizan tanto las campañas en estado de borrador como las activas que el usuario puede administrar.
Creación de Campaña (/create)
Un proceso guiado permite a los usuarios crear nuevas campañas, definiendo:
Objetivos de recaudación mínimos y máximos (en USD, convertidos a ADA)
Fechas de inicio y finalización de la campaña de recaudación
Cronograma de hitos (2-5 milestones)
Precio de tokens
Información del equipo y proyecto
Materiales promocionales
Página de Detalles de Campaña (/campaign?id=xx) 
Cada campaña cuenta con una página detallada que muestra toda la información relevante:
Descripción y objetivos del proyecto
Estado actual de la recaudación
Hitos y su progreso
Actualizaciones del equipo
Historial de transacciones
Comentarios y discusiones
Se muestra ya renderizada la campaña, esté aprobada o no aún, y se muestran botones de administración correspondientes al estado y al usuario conectado.
Existe un botón edit, que nos lleva a la página de edición de contenido de la campaña.
Edición de Campaña (/edit?id=xx)
Permite a los usuarios actualizar y administrar la campaña
Protocolo
Descripción General
El Protocolo es el componente central del sistema de crowdfunding que actúa como el hub de control y configuración para todo el ecosistema. Gestiona la gobernanza, establece las reglas base y mantiene la configuración global del sistema.
Representación en la Red
El Protocolo está representado por una sola UTxO con el Datum del Protocolo, siguiendo el patrón de "único punto de verdad" para la configuración global del sistema.
Datum del Protocolo
Estructura que representa el estado del protocolo en la blockchain:
pdProtocolVersion: Integer
Versión actual del datum del protocolo
pdAdmins: [String]
Lista de payment public key hashes del Core Team que pueden administrar el protocolo y las campañas
pdTokenAdminPolicy_CS: String
Hash de la política del token de administración del protocolo
pdMinADA : Integer
Cantidad mínima de ADA requerida en el UTXO del protocolo
Nota: Este datum contiene la configuración fundamental del protocolo, incluyendo los permisos administrativos y parámetros base del sistema. Su diseño como UTxO único permite un control centralizado mientras mantiene la naturaleza descentralizada de la blockchain
Responsabilidades Principales
Gobernanza y Control:
Gestión de administradores (Core Team)
Control de políticas de tokens administrativos
Definición de reglas y parámetros base
Configuración del Sistema:
Versión del protocolo
Uso de mínimos de ADA
Políticas de tokens y validadores
Validación y Seguridad:
Verificación de operaciones
Control de acceso mediante tokens
Registro inmutable de operaciones
Representación en la DB
Ver detalle de la entidad aquí
Interacciones Del Protocolo
Create Protocol Contracts (DB)
Descripción: Crea los contratos inteligentes del protocolo y los almacena en la base de datos. Se crean Protocol ID Policy, Protocol Validator, Campaign Validator y Campaign Funds Validator.
Ejecuta: Protocol Team
Initialize Protocol Contracts  (TX)
Descripción: Crea el datum inicial del protocolo en la blockchain con parámetros por defecto que serán utilizados por todas las campañas
Ejecuta: Protocol Team
Publish Protocol Contracts (TX)
Descripción: Despliega los contratos en la blockchain creando UTxOs que servirán como referencia para futuras operaciones
Ejecuta: Protocol Team
Remove Protocol Contracts (TX)
Descripción: Se eliminan las UTxOs con los contratos y permite recuperar el ADA reservado. Solo se pueden eliminar si no hay campañas creadas.
Ejecuta: Protocol Team
Sync (DB)
Descripción: Fuerza la sincronización de la entidad en la base de datos y la red
Ejecuta: Protocol Team

Campañas
Representa un proyecto de crowdfunding con presencia dual:
En Blockchain:
Objetivos de recaudación
Estado y fechas críticas
Configuración de milestones
Registros de transacciones
Billeteras autorizadas
En Base de Datos:
Descripción detallada
Roadmap completo
Información del equipo
Materiales multimedia
Estado de la campaña
Actualizaciones y comunicaciones
Comentarios y discusiones
Representación en la Red
Cada campaña estará representada por una UTxO con el datum de campaña en el contrato de campañas y al menos una UTxO de fondos por cada campaña.
Datum de la Campaña
Estructura que representa el estado de una campaña en la blockchain:
cdCampaignVersion : Integer
Versión actual del datum de la campaña
cdCampaignPolicy_CS : String
Hash de la política que controla el token ID de la campaña
cdCampaignFundsPolicyID_CS : String
Hash de la política que controla los tokens ID de fondos de la campaña
cdAdmins : [String]
Lista de payment public key hashes de los administradores
cdTokenAdminPolicy_CS : String
Hash de la política del token de administración
cdMint_CampaignToken : Boolean
Indica si la campaña acuñará sus propios tokens
cdCampaignToken_CS : String
Hash de la política del token de la campaña
cdCampaignToken_TN : String
Nombre del token de la campaña
cdCampaignToken_PriceADA : Integer
Precio en ADA de cada token de la campaña
cdRequestedMaxADA : Integer
Cantidad máxima de ADA a recaudar.  Una vez alcanzados no se venden más tokens
cdRequestedMinADA : Integer
Cantidad mínima de ADA a recaudar.  Una vez superada esta cantidad la campaña se considera cumplida. 
cdFundedADA : Integer
Cantidad total de ADA recaudada
cdCollectedADA : Integer
Cantidad de ADA retirada por los administradores
cdBeginAt : Date
Fecha de inicio de la campaña
cdDeadline : Date
Fecha límite de la campaña
cdStatus : CampaignStatus
Estado actual de la campaña
cdMilestones : [CampaignMilestones]
Lista de hitos de la campaña
cdFundsCount : Integer
Cantidad total de UTXOs de fondos
cdFundsIndex : Integer
Índice para generar nuevos IDs de fondos
cdMinADA : Integer
Cantidad mínima de ADA requerida en el UTXO
Cada milestone (CampaignMilestones) en el datum está formado por:
cmEstimatedDeliveryDate : Date
Fecha estimada de entrega del milestone. No se usa a nivel código
cmPercentage : Integer
Porcentaje sobre el total de los fondos ADA acumulados que representa este milestone
cmStatus : MilestoneStatus
Estado actual del milestone, indica su progreso (Created, Success, Failed)
Nota: Los milestones son utilizados para controlar la liberación progresiva de fondos basada en el cumplimiento de objetivos del proyecto.
Representación en la DB
Ver detalle de la entidad aquí
Estados de Campañas
Los estados (status) en los datums son los que los contratos van a observar para permitir ciertas acciones: la venta de tokens, la recuperación de las ADA si una campaña falla, el cobro de los milestones, etc. 
Los estados en los datums se modifican con transacciones que podrán hacer los managers o el protocolo.
Estos estados además pueden ser combinados con otros valores del datum para determinar estados más específicos. Por ej, si una campaña está CsInitialized pero la fecha de Inicio aún no ha llegado, la campaña está en un estado lógico de en Countdown, pero si la fecha ya ha llegado, la campaña está en Fundraising. 
Estos estados inferidos, nuevos, se guardan en la base de datos.
Estados en el datum 
CsCreated: apenas creado
CsInitialized: con tokens disponibles a la venta
CsReached: fondos mínimos alcanzados
CsNotReached: fondos mínimos no alcanzados
CsFailedMilestone: milestone no cumplido

De los milestones son:
MsCreated: apenas creado
MsSuccess: aprobado
MsFailed: fallado
Estados en la base de datos 
Created: Campaña creada por algún usuario.
Submitted: El administrador de la campaña notifica que la misma está lista para revisión por parte del equipo del Protocolo..
Rejected: No aprobada por el equipo.
Approved: Aprobada por el equipo
Contract Created: Se crearon los smart contracts.
Contract Published: Los smart contract se agregaron a la red.
Contract Started: Se desplegó UTxO con datum de campaña en la red (en datum: CsCreated).
Countdown: Desde el día actual a la fecha de inicio. (en datum: CsInitialized).
Fundraising: Desde fecha de inicio hasta la fecha de finalización. Se permiten hacer la venta de tokens a los usuarios (en datum: CsInitialized).
Finishing:  Luego de la fecha de finalización. Estado intermedio entre que termina la campaña y los admins del Protocolo la marcan como CsReached o CsNotReached (en datum: CsInitialized).
Active: Se consiguió la meta de financiamiento y la campaña está desarrollando las milestones (en datum: CsReached y milestone status va de los subsiguientes milestones van a ir pasando de MsCreated a MsSuccess or MsFailed). Este estado se complementa con el estado del milestone activo, que pueden ser: Created, Submitted, Rejected, Success Failed
Success: Que se cumplieron todos los milestones (en datum: tiene que tener todos los milestones MsSuccess)
Failed: Que falló el cumplimiento de algún milestone, una vez cumplida la meta de recaudación (en datum: CsFailedMilestone y algún milestone status en: MsFailed), 
Unreached: Que no alcanzó la meta de financiación y el proyecto no pudo comenzar. (en datum: CsNotReached)

De los milestones son:
Not Started: Campaña no es Active o este milestone aún no inicia por que se está trabajando en uno anterior (en milestone datum: MsCreated)
Started: Campaña es Active y los milestones anteriores son Finished (y este milestone datum: MsCreated)
Submitted: En la base de datos se podrá marcar un milestone como Submitted para solicitar su revisión. Campaña es Active y los milestones anteriores son Finished (y este milestone datum: MsCreated)
Rejected: En la base de datos se podrá marcar un milestone como rejected para que lo vuelvan a presentar. Campaña es Active y los milestones anteriores son Finished (y este milestone datum: MsCreated)
Finished: Campaña es Active y los milestones anteriores son: Finished (y este milestone datum: MsSuccess)
Failed: Campaña es Failed y los milestones anteriores son: Finished (en milestone datum: MsFailed)
Campañas archivadas
Es un flag adicional en la base de datos:
Archived: Se puede marcar una campaña para no ser mostrada en el home ni en . Se muestra sólo en Manage.
Campañas destacadas
Es un flag adicional en la base de datos:
Featured: Se puede marcar una campaña a este estado para no ser mostrada en el home. Se muestra en More Campaigns y en Manage.
Estados en el Website
Los estados de la base de datos son los que van a llegar desde el backend. El Front deberá traducirlos de la siguiente forma:
TBL (To be Launched): Sólo en la página Manage. Se está analizando de parte de los administradores del marketplace antes de aceptarse o faltan los contratos o las transacciones de inicialización. 
Estados que se incluyen: 
TBL (Created)
TBL (Submitted)
TBL (Approved)
TBL (Contract Created)
TBL (Contract Published)
TBL (Contract Started) 
Rejected: Sólo en Manage.
TBL (Rejected)
Countdown 
Fundraising 
Finishing
Active:  Según el estado de sus milestones se puede inferir un estado más concreto: Active (M1), Active (M1 Submitted), Active (M1 Rejected), o algo así.
Success
Failed
Unreached
Archived: Cualquier campaña en cualquier estado, pero con el flag Archived activado. Sólo aparecen en Manage.

Los status de los milestone se van a mostrar en el sitio como:
Not Started
Started
Submitted
Rejected
Finished
Failed
Proceso de Creación y Gestión de Campañas
Overview
Los Managers crean campañas en la plataforma
Los Managers revisan y marcan la campaña como lista para revisión
El equipo del Protocol aprueba la campaña, crea, pública e inicializa los contratos
El Protocol o los Managers crean las UTxO de fondos y depositan los tokens para la venta
El Protocol o los Managers activan la campaña (CsInitialized), iniciando el período de Countdown y posteriormente Fundraising
Los usuarios participan comprando tokens a cambio de ADA durante el Fundraising
El Protocol o los Managers determinan el resultado (CsReached o CsNotReached) al finalizar el período
Los Managers reciben acceso a los fondos del primer milestone
Los Managers presentan evidencias y deliverables de los avances
El Protocol revisa y valida o rechaza el cumplimiento del milestone
Los Managers acceden al siguiente milestone si es aprobado y entregan nuevos deliverables
En caso de fallo, el Protocolo los Managers pueden recuperar los tokens no vendidos
En caso de fallo, los usuarios pueden devolver sus tokens para recuperar su ADA
Creación Inicial
Cuando un usuario crea una campaña:
Debe conectar su billetera a través del Wallet Connector
Puede opcionalmente vincular un email si no lo ha hecho
Crea la campaña en los formularios paso a paso
Define emails de editores para gestión de contenido
Define las billeteras que tendrán rol de Managers (se registran en el Datum)
La campaña se crea inicialmente solo en la base de datos
Los Managers o Editores envían la campaña a revisión.
Aprobación y Despliegue
El Protocol Team realiza una revisión exhaustiva que incluye:
Verificación de identidades y credenciales
Análisis de viabilidad del proyecto
Revisión de estructura de hitos
Validación de objetivos financieros
Solo tras la aprobación:
Se despliegan los smart contracts necesarios
Se registran las billeteras autorizadas en el Datum
La campaña queda visible en el marketplace
Se habilita la compra de tokens
Hitos y Validación
Los managers o editores presentan evidencia del cumplimiento de hitos
El equipo del protocolo revisa y valida cada hito
La aprobación del hito libera automáticamente los fondos correspondientes
Gestión Continua
La gestión diaria de la campaña combina:
Operaciones on-chain ejecutadas por Managers mediante sus billeteras
Actualizaciones de contenido realizadas por Editores identificados por email
Validaciones del Protocol Team para hitos y operaciones críticas
Mecanismos de Seguridad
El protocolo implementa sistemas de protección para los inversores:
Devolución de fondos si no se alcanza el mínimo en caso de fallo de hitos
Transparencia total de transacciones y estados
Esquema de pago
Las campañas pueden recaudar hasta su máximo establecido como objetivo.
Si una campaña alcanza el mínimo, los fondos recaudados quedan bloqueados para ese proyecto.
Cada milestone representa un porcentaje del total recaudado.
Los creadores pueden cobrar los fondos de cada milestone una vez que el Protocol aprueba su cumplimiento.
Los fondos se liberan de manera secuencial: debe completarse un milestone para acceder al siguiente.
En caso de fallo de un milestone, los fondos restantes quedan disponibles para que los inversores recuperen su ADA.
Interacciones de Campañas
Gestión Inicial de Campaña
Create Campaign (DB)
Descripción: Crea una nueva campaña en la base de datos. Define información básica como título, descripción, objetivos de financiación, cronograma de milestones y detalles del equipo
Estado Inicial DB: N/A
Estado Final DB: Created
Ejecuta: Campaign Managers
Send to Revision (DB)
Descripción: Marca la campaña como lista para revisión por parte del Protocol Team. Crea una entrada en la tabla Campaign-Submission
Estado Inicial DB: Created/Rejected
Estado Final DB: Submitted 
Ejecuta: Campaign Managers, Campaign Editors
Approve Campaign (DB)
Descripción: Aprueba la campaña para su despliegue en blockchain
Estado Inicial DB: Submitted
Estado Final DB: Approved
Ejecuta: Protocol Team
Reject Campaign (DB)
Descripción: Rechaza la campaña para que sea revisada y modificada
Estado Inicial DB: Submitted
Estado Final DB: Rejected
Ejecuta: Protocol Team
Despliegue de Contratos de Campaña
Create Campaign Contracts (DB)
Descripción: Crea los contratos específicos de la campaña (Campaign Policy y Campaign Funds Policy) y los almacena en la base de datos
Estado Inicial DB: Approved
Estado Final DB: Contract Created
Ejecuta: Protocol Team
Initialize Campaign Contracts (TX)
Descripción: Crea el datum inicial de la campaña en la blockchain
Estado Inicial DB: Contract Published (Milestones: Not Started)
Estado Final DB: Contract Started (Milestones: Not Started)
Estado Inicial Datum: N/A (Milestones: N/A)
Estado Final Datum: CsCreated (Milestones: MsCreated)
Ejecuta: Protocol Team
Publish Campaign Contracts (TX)
Descripción: Despliega los contratos de la campaña en la blockchain
Estado Inicial DB: Contract Created
Estado Final DB: Contract Published
Ejecuta: Protocol Team, Campaign Managers
Remove Campaign Contracts (TX)
Descripción: Se eliminan las UTxOs con los contratos y permite recuperar el ADA reservado. Si se ejecuta cuando la campaña está en Contract Published esta pasa a Contract Created. Si se ejecuta al final de la vida de la campaña (Success, Failed or Unreached) el estado no se modifica. Solo se pueden eliminar si no hay tokens ni ADA alojados.
Estado Inicial DB: Contract Published
Estado Final DB: Contract Created
Ejecuta: Protocol Team, Campaign Managers



Gestión de Campaña
Launch Campaign (TX)
Descripción: 
Activa la campaña para recaudación de fondos. El contrato verifica que la cantidad de tokens puestos a la venta coincidan con los establecidos para esa campaña (máximo esperado).
Countdown, Fundraising y Finishing: Estos cambios de estado para mostrar suceden automáticamente e indican que cuanto falta para la fecha de inicio o si ya ha comenzado el fundraising o si ya ha terminado
Countdown: Desde el día actual a la fecha de inicio.
Fundraising: Desde fecha de inicio hasta la fecha de finalización. Se permiten hacer la venta de tokens a los usuarios.
Finishing:  Luego de la fecha de finalización. Estado intermedio entre que termina la campaña y los admins del Protocolo la marcan como CsReached o CsNotReached
Estado Inicial DB: Contract Started
Estado Final DB: Countdown/Fundraising/Finishing (según fecha)
Estado Inicial Datum: CsCreated
Estado Final Datum: CsInitialized
Ejecuta: Protocol Team, Campaign Managers
Invest (TX)
Descripción: Permite a inversores comprar tokens de la campaña y depositan ADA
Estado Inicial DB: Fundraising
Estado Final DB: Mismo estado
Estado Inicial Datum: CsInitialized
Estado Final Datum: Mismo estado
Ejecuta: Investors
Campaign Reached (TX)
Descripción: Marca campaña como exitosa en alcanzar objetivo mínimo. El contrato verifica que la cantidad de tokens vendidos supere el mínimo esperado
Estado Inicial DB: Finishing (Milestone 1: Not Started)
Estado Final DB: Active (Milestone 1: Started)
Estado Inicial Datum: CsInitialized (Milestone 1: MsCreated)
Estado Final Datum: CsReached (Milestone 1: MsCreated)
Ejecuta: Protocol Team, Campaign Managers
Campaign Not Reached (TX)
Descripción: Marca campaña como fallida en alcanzar objetivo mínimo. El contrato verifica que la cantidad de tokens vendidos no supera el mínimo esperado. 
Estado Inicial DB: Finishing
Estado Final DB: Unreached
Estado Inicial Datum: CsInitialized
Estado Final Datum: CsNotReached
Ejecuta: Protocol Team, Campaign Managers
Update Campaign (DB/TX)
Descripción: Actualiza la información de la campaña en la base de datos y datum. Los campos editables dependen del estado de la campaña y el rol del usuario
Estado Inicial DB: Mismo estado
Estado Final DB: Mismo estado
Ejecuta: Protocol Team, Campaign Managers, Campaign Editors (sólo campos en DB)
Update Campaign Min ADA (TX)
Descripción: Actualiza el requerimiento mínimo de ADA en Campaign UTxO
Estado Inicial DB: Contract Started o posterior
Estado Final DB: Mismo estado
Estado Inicial Datum: CsCreated o posterior
Estado Final Datum: Mismo estado
Ejecuta: Protocol Team, Campaign Managers
Delete Campaign (TX/DB)
Descripción: Elimina campaña si no tiene tokens/ADA alojados
Estado Inicial DB: Created o después de Success/Failed/Unreached
Estado Final DB: Eliminada
Restricción: No disponible durante fundraising activo
Ejecuta: Protocol Team, Campaign Managers (solo en Created)
Gestión de Campañas Fallidas
Return Tokens (TX)
Descripción: Permite a inversores devolver tokens para recuperar parte o la totalidad de su inversión en ADA, dependiendo si la campaña ya había consumido fondos o no.
Estado Inicial DB: Unreached/Failed
Estado Final DB: Mismo estado
Estado Inicial Datum: CsNotReached/CsFailedMilestone
Estado Final Datum: Mismo estado
Ejecuta: Investors
Gestión de Fondos
Add Campaign Funds UTxO (TX)
Descripción: Crea nueva UTxO para gestionar fondos de la campaña. Puede haber múltiples UTxOs para evitar problemas de concurrencia
Estado Inicial DB: Contract Started o posterior
Estado Final DB: Mismo estado
Estado Inicial Datum: CsCreated o posterior
Estado Final Datum: Mismo estado
Ejecuta: Protocol Team, Campaign Managers
Deposit Campaign Tokens (TX)
Descripción: Deposita y/o mintea tokens para venta en los UTxOs de fondos
Estado Inicial DB: Contract Started
Estado Final DB: Mismo estado
Estado Inicial Datum: CsCreated
Estado Final Datum: Mismo estado
Ejecuta: Protocol Team, Campaign Managers
Withdraw Campaign Tokens (TX)
Descripción: Retira y/o quema tokens de venta de los UTxOs de fondos. Es útil cuando la campaña falla para poder recuperar los tokens puestos en venta.
Estado Inicial DB: Contract Started/Unreached/Failed  
Estado Final DB: Mismo estado
Estado Inicial Datum: CsCreated/CsNotReached/CsFailedMilestone
Estado Final Datum: Mismo estado
Ejecuta: Protocol Team, Campaign Managers
Merge Fund UTxOs (TX)
Descripción: Combina múltiples UTxOs de fondos en uno solo
Estado Inicial DB: Contract Started o posterior
Estado Final DB: Mismo estado
Estado Inicial Datum: CsCreated o posterior
Estado Final Datum: Mismo estado
Ejecuta: Protocol Team, Campaign Managers
Balance Fund UTxOs (TX)
Descripción: Redistribuye tokens entre diferentes UTxOs de fondos
Estado Inicial DB: Contract Started o posterior
Estado Final DB: Mismo estado
Estado Inicial Datum: CsCreated o posterior
Estado Final Datum: Mismo estado
Ejecuta: Protocol Team, Campaign Managers
Update Funds Min ADA (TX)
Descripción: Actualiza el requerimiento mínimo de ADA en Funds UTxOs
Estado Inicial DB: Contract Started o posterior
Estado Final DB: Mismo estado
Estado Inicial Datum: CsCreated o posterior
Estado Final Datum: Mismo estado
Ejecuta: Protocol Team, Campaign Managers
Delete Fund UTxOs (TX)
Descripción: Elimina UTxOs de fondos. Se validará que no haya tokens ni ADA alojados.
Estado Inicial DB: Contract Started o posterior
Estado Final DB: Mismo estado
Estado Inicial Datum: Cualquiera
Estado Final Datum: Mismo estado
Ejecuta: Protocol Team, Campaign Managers
Gestión de Milestones
Send Report Milestone (DB)
Descripción: Presenta reporte de completitud del milestone actual
Estado Inicial DB: Active (Milestone actual: Started)
Estado Final DB: Active (Milestone actual: Submitted)
Estado Inicial Datum: CsReached (Milestone actual: MsCreated)
Estado Final Datum: Mismo estado
Ejecuta: Campaign Managers, Campaign Editors
Approve Milestone (TX)
Descripción: Aprueba completitud del milestone actual y permite cobro de fondos
Estado Inicial DB: Active (Milestone actual: Submitted, Milestone siguiente: Not Started)
Estado Final DB: Active (Milestone actual: Finished, Milestone siguiente: Started)
Estado Inicial Datum: CsReached (Milestone actual: MsCreated)
Estado Final Datum: CsReached (Milestone actual: MsSuccess)
Ejecuta: Protocol Team
Reject Milestone (DB)
Descripción: Rechaza presentación del milestone actual para revisión
Estado Inicial DB: Active (Milestone actual: Submitted)
Estado Final DB: Active (Milestone actual: Rejected)
Estado Inicial Datum: CsReached (Milestone actual: MsCreated)
Estado Final Datum: Mismo estado
Ejecuta: Protocol Team
Fail Milestone (TX)
Descripción: Falla permanentemente el milestone, fallando la campaña
Estado Inicial DB: Active (Milestone actual: Submitted/Rejected)
Estado Final DB: Failed
Estado Inicial Datum: CsReached (Milestone actual: MsCreated)
Estado Final Datum: CsFailedMilestone (Milestone actual: MsFailed)
Ejecuta: Protocol Team
Collect Funds (TX)
Descripción: Cobra fondos liberados tras aprobación de milestone
Estado Inicial DB: Active (Milestone actual: Finished)
Estado Final DB: Mismo estado
Estado Inicial Datum: CsReached (Milestone actual: MsSuccess)
Estado Final Datum: Mismo estado
Ejecuta: Campaign Managers
Gestión Administrativa
Feature / UnFeature Campaign (DB)
Descripción: Marca campaña como si/no destacada en el marketplace
Estado Inicial DB: Countdown o posterior
Estado Final DB: Mismo estado (con flag Featured si/no)
Estado Inicial Datum: CsInitialized o posterior
Estado Final Datum: Mismo estado
Ejecuta: Protocol Team
Archive / UnArchive Campaign (DB)
Descripción: Oculta/Muestra la campaña de los listados
Estado Inicial DB: Cualquiera
Estado Final DB: Mismo estado (con flag Archived si/no)
Ejecuta: Protocol Team
Interacciones por Rol
Protocol Team
Gestión Inicial de Campaña
Approve Campaign (DB)
Reject Campaign (DB)
Despliegue de Contratos de Campaña
Create Campaign Contracts (DB)
Initialize Campaign Contracts (TX)
Publish Campaign Contracts (TX)
Remove Campaign Contracts (TX)
Gestión de Campaña
Launch Campaign (TX)
Campaign Reached (TX)
Campaign Not Reached (TX)
Update Campaign (DB/TX)
Update Campaign Min ADA (TX)
Delete Campaign (TX/DB)
Gestión de Fondos
Add Campaign Funds UTxO (TX)
Deposit Campaign Tokens (TX)
Withdraw Campaign Tokens (TX)
Merge Fund UTxOs (TX)
Balance Fund UTxOs (TX)
Update Funds Min ADA (TX)
Delete Fund UTxOs (TX)
Gestión de Milestones
Approve Milestone (TX)
Reject Milestone (DB)
Fail Milestone (TX)
Gestión Administrativa
Feature / UnFeature Campaign (DB)
Archive / UnArchive Campaign (DB)


Campaign Managers
Gestión Inicial de Campaña
Create Campaign (DB)
Send to Revision (DB)
Despliegue de Contratos de Campaña
Publish Campaign Contracts (TX)
Remove Campaign Contracts (TX)
Gestión de Campaña
Launch Campaign (TX)
Campaign Reached (TX)
Campaign Not Reached (TX)
Update Campaign (DB/TX)
Update Campaign Min ADA (TX)
Delete Campaign (TX/DB) - Solo en Created
Gestión de Fondos
Add Campaign Funds UTxO (TX)
Deposit Campaign Tokens (TX)
Withdraw Campaign Tokens (TX)
Merge Fund UTxOs (TX)
Balance Fund UTxOs (TX)
Update Funds Min ADA (TX)
Delete Fund UTxOs (TX)
Gestión de Milestones
Send Report Milestone (DB)
Collect Funds (TX)
Campaign Editors
Gestión Inicial de Campaña
Send to Revision (DB)
Gestión de Campaña
Edit Campaign (DB) - Solo datos off-chain
Gestión de Milestones
Send Report Milestone (DB)
Investors
Gestión de Campaña
Invest (TX)
Return Tokens (TX)
Interacciones por Estado
Referencia de Modal de Fund Management Actions:
Add Campaign Funds UTxO (TX)
Deposit Campaign Tokens (TX)
Withdraw Campaign Tokens (TX)
Merge Fund UTxOs (TX)
Balance Fund UTxOs (TX)
Update Funds Min ADA (TX)
Delete Fund UTxOs (TX)

Referencia de Modal de Milestone Review Actions:
Approve Milestone (TX)
Reject Milestone (DB)
Fail Milestone (TX)

En proceso de creación
Aún no está presente en la base de datos. Se mantiene en memoria en el browser mientras el creador termina de completar los campos básicos.

Se muestra en las tarjetas / campaña renderizada:
Home: 
no se muestra
Onboarding Campaign Creator:
Botones disponibles en tarjeta y/o  campaña renderizada:
Save (DB)
Campaign Managers
Created
Fué creada por algún usuario y aún puede editar el contenido libremente.

Se muestra en las tarjetas / campaña renderizada:
Home: 
no se muestra
Manage:
‘Draft (Created)’:
Botones disponibles en tarjeta:
View Campaign: va a campaña renderizada
Send to Revision (DB) 
Campaign Managers
Campaign Editors
Campaña renderizada:
Send to Revision (DB)
Campaign Managers
Campaign Editors
Edit Campaign (DB)
Protocol Team
Campaign Managers
Campaign Editors
Delete Campaign (DB)
Protocol Team
Campaign Managers
Archive/UnArchive Campaign (DB)
Protocol Team
Submitted
Fue enviada a revisión por los Managers o Editores. 

Se muestra en las tarjetas / campaña renderizada:
Home: 
no se muestra
Manage:
Tarjeta:
Managers:
‘Submitted’:
View Campaign: va a campaña renderizada
Protocol Team:
‘Attempt to Launch’:
View Campaign: va a campaña renderizada
Manage Campaign: modal de aprobar o rechazar (Approve Campaign (DB) y Reject Campaign (DB))
Campaña renderizada:
Managers:
Submitted: ningun boton
Protocol Team:
Manage Campaign: modal de aprobar o rechazar (Approve Campaign (DB) y Reject Campaign (DB))
Edit Campaign (DB)
Delete Campaign (DB)
Archive/UnArchive Campaign (DB)

Rejected
No fué aprobada por el Protocolo. Debe ser revisada, editada y enviada a revisión nuevamente.

Se muestra en las tarjetas / campaña renderizada:
Home: 
no se muestra
Manage:
Tarjeta:
Managers  y Editors:
‘Rejected’:
View Report: va a campaña renderizada en la tab submissions
Protocol Team:
‘Rejected’:
View Campaign: va a campaña renderizada
Edit Campaign (DB)
Campaña renderizada:
Managers y Editors:
‘Rejected’:
Edit Campaign (DB)
Send to Revision (DB)
Protocol Team:
‘Rejected’:
Edit Campaign (DB)
Delete Campaign (DB)
Archive/UnArchive Campaign (DB)

Approved
Fué aprobada por el Protocolo. Ya no se puede editar el contenido sensible de la campaña. El Protocolo deberá crear los contratos.

Se muestra en las tarjetas / campaña renderizada:
Home: 
no se muestra
Manage:
Tarjeta:
Managers:
‘Approved’:
View Campaign: va a campaña renderizada
Protocol Team:
‘Approved’:
View Campaign: va a campaña renderizada
Create Campaign Contracts (DB)
Campaña renderizada:
Managers:
‘Approved’: ningun boton
Protocol Team:
‘Approved’:
Create Campaign Contracts (DB)
Edit Campaign (DB)
Delete Campaign (DB)
Archive/UnArchive Campaign (DB)
Contract Created
Los contratos fueron creados para esta campaña. El Protocolo deberá publicar los contratos.

Se muestra en las tarjetas / campaña renderizada:
Home: 
no se muestra
Manage:
Tarjeta:
Managers:
‘Created’:
View Campaign: va a campaña renderizada
Protocol Team:
‘Created’:
View Campaign: va a campaña renderizada
Publish Campaign Contracts (TX)
Campaña renderizada:
Managers:
‘Created’: ningun boton
Protocol Team:
‘Created’:
Publish Campaign Contracts (TX)
Edit Campaign (DB)
Delete Campaign (DB)
Archive/UnArchive Campaign (DB)
Contract Published
Los contratos fueron publicados. El Protocolo deberá inicializar los contratos. Se creará el Datum de la Campaña.

Se muestra en las tarjetas / campaña renderizada:
Home: 
no se muestra
Manage:
Tarjeta:
Managers:
‘Deploy’:
View Campaign: va a campaña renderizada
Protocol Team:
‘Deploy’:
View Campaign: va a campaña renderizada
Initialize Campaign Contracts (TX): Abre modal con todos los campos que van al datum y boton aceptar
Campaña renderizada:
Managers:
‘Deploy’: ningun boton
Protocol Team:
‘Deploy’:
Initialize Campaign Contracts (TX):  Abre modal con todos los campos que van al datum y boton aceptar
Edit Campaign (DB)
Delete Campaign (DB)
Archive/UnArchive Campaign (DB)
Contract Started
Los contratos fueron iniciados. El Protocolo o Managers deberán iniciar la campaña cuando hayan creado las UTxOs de Fondos y haya dejado los tokens necesarios a la venta.

Se muestra en las tarjetas / campaña renderizada:
Se muestra en las tarjetas / campaña renderizada:
Home: 
no se muestra
Manage:
Tarjeta:
Managers:
‘Ready’:
View Campaign: va a campaña renderizada
Launch Campaign (TX)
Protocol Team:
‘Ready’:
View Campaign: va a campaña renderizada
Launch Campaign (TX)
Campaña renderizada:
Managers:
‘Ready’:
Launch Campaign (TX)
Update Campaign Min ADA (TX)
Manage Funds (TX): abre modal con todo lo que se necesita para 
Protocol Team:
‘Ready’:
Launch Campaign (TX)
Update Campaign Min ADA (TX)
Manage Funds (TX): abre modal con todo lo que se necesita para 
Edit Campaign (DB)
Delete Campaign (DB)
Archive/UnArchive Campaign (DB)

Countdown
La campaña fue iniciada pero la fecha de comienzo aún no llega.

Se muestra en las tarjetas / campaña renderizada:
Home: 
Tarjeta:
‘Contador de tiempo’:
Learn More: va a campaña renderizada
Campaña renderizada:
‘Contador de tiempo’:
Ningun Boton

Manage:
Tarjeta:
Managers:
Contador de tiempo:
View Campaign: va a campaña renderizada
Protocol Team:
‘Contador de tiempo’:
View Campaign: va a campaña renderizada
Campaña renderizada:
Managers:
Contador de tiempo:
Update Campaign Min ADA (TX)
Manage Funds (TX): abre modal con todo lo que se necesita para 
Protocol Team:
Contador de tiempo:
Update Campaign Min ADA (TX)
Manage Funds (TX): abre modal con todo lo que se necesita para 
Edit Campaign (DB)
Delete Campaign (DB)
Archive/UnArchive Campaign (DB)
Fundraising
La campaña fue iniciada y la fecha de comienzo ya pasó. Se inicia formalmente la venta de tokens.





Se muestra en las tarjetas / campaña renderizada:
Home: 
Tarjeta:
‘Fundraising’:
Learn More: va a campaña renderizada
Invest (TX)
Campaña renderizada:
‘Fundraising’:
Invest (TX)
Manage:
Tarjeta:
Managers:
Fundraising:
View Campaign: va a campaña renderizada
Protocol Team:
‘Fundraising’:
View Campaign: va a campaña renderizada
Campaña renderizada:
Managers:
Fundraising:
Update Campaign Min ADA (TX)
Manage Funds (TX): abre modal con todo lo que se necesita para 
Protocol Team:
Fundraising:
Update Campaign Min ADA (TX)
Manage Funds (TX): abre modal con todo lo que se necesita para 
Edit Campaign (DB)
Delete Campaign (DB)
Archive/UnArchive Campaign (DB)
Finishing
Se terminó la campaña por que la fecha de finalización ya pasó. Este es un estado intermedio donde algún usuario debería marcar la campaña como Reached o NotReached. Los contratos validaron esa acción comprobando la cantidad de tokens vendidos y los mínimos esperados.

Se muestra en las tarjetas / campaña renderizada:

Home: 
no se muestra
Manage:
Tarjeta:
Managers:
Finishing:
Validate Fundraising Status (TX): va a llamar a:
Campaign Reached (TX)
Campaign Not Reached (TX)
Protocol Team:
Finishing:
Validate Fundraising Status (TX): va a llamar a:
Campaign Reached (TX)
Campaign Not Reached (TX)
Campaña renderizada:
Managers:
Finishing:
Validate Fundraising Status (TX)
Update Campaign Min ADA (TX)
Manage Funds (TX): abre modal con todo lo que se necesita para 
Protocol Team:
Finishing:
Validate Fundraising Status (TX)
Update Campaign Min ADA (TX)
Manage Funds (TX): abre modal con todo lo que se necesita para 
Edit Campaign (DB)
Delete Campaign (DB)
Archive/UnArchive Campaign (DB)
Active
La campaña fué un éxito en recaudación y se marcó como Reached. Ahora es tiempo de llevar adelante el desarrollo del proyecto.

Se muestra en las tarjetas / campaña renderizada:

Home: 
Tarjeta:
‘Active’:
View Roadmap: va a campaña renderizada tab roadmap
Learn More: va a campaña renderizada
Campaña renderizada:
‘Active’:
Ningun Boton
Manage:
Tarjeta:
Managers y Editors:
‘Active’ (cuando hay un milestone en proceso:
View Campaign: va a campaña renderizada
Send Report Milestone (DB)
‘Reported’ (cuando ya envió el reporte):
View Report: va a campaña renderizada
‘Rejected’ (cuando fue rechazado):
View Report: va a campaña renderizada
Send Report Milestone (DB)
‘Collect’ (cuando fue aprobado):
View Campaign: va a campaña renderizada
Collect Funds (TX): solo managers
Protocol Team:
‘Active’ (cuando hay un milestone en proceso:
View Campaign: va a campaña renderizada
‘Reported’ (cuando ya envió el reporte):
View Campaign: va a campaña renderizada
Manage Milestone: Modal de Milestone Review
‘Rejected’ (cuando fue rechazado):
View Campaign: va a campaña renderizada
Manage Milestone: Modal de Milestone Review
‘Collect’ (cuando fue aprobado):
View Campaign: va a campaña renderizada

Campaña renderizada:
Managers y Editors:
‘Active’ (cuando hay un milestone en proceso:
Send Report Milestone (DB)
Update Campaign Min ADA (TX)
Manage Funds (TX): abre modal con todo lo que se necesita para 
‘Reported’ (cuando ya envió el reporte):
Update Campaign Min ADA (TX)
Manage Funds (TX): abre modal con todo lo que se necesita para 
‘Rejected’ (cuando fue rechazado):
Send Report Milestone (DB)
Update Campaign Min ADA (TX)
Manage Funds (TX): abre modal con todo lo que se necesita para 
‘Collect’ (cuando fue aprobado):
Collect Funds (TX): solo managers
Update Campaign Min ADA (TX)
Manage Funds (TX): abre modal con todo lo que se necesita para 
Protocol Team:
‘Active’ (cuando hay un milestone en proceso:
Update Campaign Min ADA (TX)
Manage Funds (TX): abre modal con todo lo que se necesita para 
Edit Campaign (DB)
Delete Campaign (DB)
Archive/UnArchive Campaign (DB)
‘Reported’ (cuando ya envió el reporte):
Manage Milestone: Modal de Milestone Review
Update Campaign Min ADA (TX)
Manage Funds (TX): abre modal con todo lo que se necesita para 
Edit Campaign (DB)
Delete Campaign (DB)
Archive/UnArchive Campaign (DB)
‘Rejected’ (cuando fue rechazado):
Manage Milestone: Modal de Milestone Review
Update Campaign Min ADA (TX)
Manage Funds (TX): abre modal con todo lo que se necesita para 
Edit Campaign (DB)
Delete Campaign (DB)
Archive/UnArchive Campaign (DB)
‘Collect’ (cuando fue aprobado):
Update Campaign Min ADA (TX)
Manage Funds (TX): abre modal con todo lo que se necesita para 
Edit Campaign (DB)
Delete Campaign (DB)
Archive/UnArchive Campaign (DB)






Failed

El milestone fué rechazado y con él también se marcó la campaña como fallada. Los Managers o Editores ya no pueden presentarlo nuevamente.

Se muestra en las tarjetas / campaña renderizada:
Home: 
Failed
Manage:
Failed

Botones disponibles en tarjeta y/o  campaña renderizada:
Get Back Tokens (TX)
Investors
Fund Management Actions:
Add Campaign Funds UTxO (TX)
Merge Fund UTxOs (TX)
Balance Fund UTxOs (TX)
Update Funds Min ADA (TX)
Delete Fund UTxOs (TX)
Withdraw Campaign Tokens (TX)
Available to:
Protocol Team
Campaign Managers
Update Campaign Min ADA (TX)
Protocol Team
Campaign Managers
Update Campaign (DB/TX)
Protocol Team
Campaign Managers
Campaign Editors (DB only)
Delete Campaign (TX/DB) - Only if no tokens/ADA
Protocol Team
Campaign Managers
Feature/UnFeature Campaign (DB)
Protocol Team
Archive/UnArchive Campaign (DB)
Protocol Team
Unreached
La campaña finalizó y no se alcanzó el mínimo esperado. Se marcó como Not Reached. Ahora es tiempo de devolver tokens.

Se muestra en las tarjetas / campaña renderizada:
Home: 
Unreached
Manage:
Unreached

Botones disponibles en tarjeta y/o  campaña renderizada:
Get Back Tokens (TX)
Investors
Fund Management Actions:
Add Campaign Funds UTxO (TX)
Merge Fund UTxOs (TX)
Balance Fund UTxOs (TX)
Update Funds Min ADA (TX)
Delete Fund UTxOs (TX)
Withdraw Campaign Tokens (TX)
Available to:
Protocol Team
Campaign Managers
Update Campaign Min ADA (TX)
Protocol Team
Campaign Managers
Update Campaign (DB/TX)
Protocol Team
Campaign Managers
Campaign Editors (DB only)
Delete Campaign (TX/DB) - Only if no tokens/ADA
Protocol Team
Campaign Managers
Feature/UnFeature Campaign (DB)
Protocol Team
Archive/UnArchive Campaign (DB)
Protocol Team
Success
El proyecto presentó todos sus milestones y fueron aprobados.

Se muestra en las tarjetas / campaña renderizada:
Home: 
Success
Manage:
Success

Botones disponibles en tarjeta y/o  campaña renderizada:
Fund Management Actions:
Add Campaign Funds UTxO (TX)
Merge Fund UTxOs (TX)
Balance Fund UTxOs (TX)
Update Funds Min ADA (TX)
Delete Fund UTxOs (TX)
Available to:
Protocol Team
Campaign Managers
Update Campaign Min ADA (TX)
Protocol Team
Campaign Managers
Update Campaign (DB/TX)
Protocol Team
Campaign Managers
Campaign Editors (DB only)
Delete Campaign (DB/TX)
Protocol Team
Feature/UnFeature Campaign (DB)
Protocol Team
Archive/UnArchive Campaign (DB)
Protocol Team
Database
La base de datos estará formada por las siguientes entidades. Todas ellas podrán ser consultadas desde el frontend con los correspondientes api calls de cada entidad. 
De acuerdo a la entidad, algunos campos relacionales producirán que la entidad recibida posea la entidad relacionada cargada, sin necesidad de realizar una consulta posterior. Eso será configurable al realizar la llamada al api de cada entidad. Se podrán elegir cuáles entidades relacionadas se quieren traer directamente desde el backend. Vamos a llamar a estos campos, campos dinámicos. No pertenecen a la base de datos, pero serán accesibles en el frontend si se decide cargar esos campos.
Protocol
Información sobre el protocolo. Esta entidad incluye todos los campos de el datum del Protocolo y agrega campos para guardar los códigos de todos los contratos.

ID-Protocol: Integer
pdProtocolVersion: Integer (datum field)
pdAdmins : [String] (datum field)
pdTokenAdminPolicy_CS: [String] (datum field)
pdMinADA: Integer (datum field)
Contratos: [String]
CreatedAt: Date
UpdatedAt: Date

Campos dinámicos:
Protocol-Admin-Wallets: [Protocol-Admin-Wallet]
Protocol-Admin-Wallet
Para relacionar el protocolo y los usuarios administradores.
Se sincroniza desde la red. Se debe actualizar el datum del protocolo.

ID-Protocol-Admin-User: Integer
Protocol-ID: Integer
Wallet-ID: Integer
Wallet: Wallet (optional)
CreatedAt: Date
UpdatedAt: Date

Campos dinámicos:
Protocol: Protocol
Campaign-Category
ID-Campaign-Category: Integer
Name: String
Description: String
CreatedAt: Date
UpdatedAt: Date
Campaign-Status
Los posibles status de las campañas.
Ver estados en la base de datos

ID-Campaign-Status: Integer
Name: String
Description: String
CreatedAt: Date
UpdatedAt: Date
Campaign-Content
Son los bloques de texto descriptivo de la campaña

ID-Campaign-Content: Integer
Campaign-ID: Integer
ID de la campaña a la que pertenece este contenido
Name: String
Description: String
Order: Integer
CreatedAt: Date
UpdatedAt: Date

Campos dinámicos:
Campaign: Campaign

Campaign-Faqs
Son los bloques de texto de Q&A de la campaña

ID-Campaign-Faqs: Integer
Campaign-ID: Integer
ID de la campaña a la que pertenece esta Q&A
Name: String
Description: String
Order: Integer
CreatedAt: Date
UpdatedAt: Date

Campos dinámicos:
Campaign: Campaign
Milestone-Status
Los posibles status de los milestones.
Ver estados en la base de datos

ID-Milestone-Status: Integer
Name: String
Description: String
CreatedAt: Date
UpdatedAt: Date
Campaign
Son las campañas creadas. Incluye todos los campos del datum y otros campos más:

ID-Campaign: Integer
Project-ID: Integer
id del proyecto al que pertenece la campaña
Campaign-Category-ID: Integer
id de la categoría de la campaña
Campaign-Status-ID: Integer 
id del estado de campaña.
Creator-Wallet-ID: Integer
id de la billetera que creó la campaña
cdCampaignVersion : Integer (datum field)
Versión actual del datum de la campaña
cdCampaignPolicy_CS : String (datum field)
Hash de la política que controla el token ID de la campaña
cdCampaignFundsPolicyID_CS : String (datum field)
Hash de la política que controla los tokens ID de fondos de la campaña
cdAdmins : [String] (datum field)
Lista de payment public key hashes de los administradores
cdTokenAdminPolicy_CS : String (datum field)
Hash de la política del token de administración
cdMint_CampaignToken : Boolean (datum field)
Indica si la campaña acuñara sus propios tokens
cdCampaignToken_CS : String (datum field)
Hash de la política del token de la campaña
cdCampaignToken_TN : String (datum field)
Nombre del token de la campaña (datum field)
cdCampaignToken_PriceADA : Integer (datum field)
Precio en ADA de cada token de la campaña
cdRequestedMaxADA : Integer (datum field)
Cantidad máxima de ADA a recaudar.  Una vez alcanzados no se venden más tokens
cdRequestedMinADA : Integer (datum field)
Cantidad mínima de ADA a recaudar.  Una vez superada esta cantidad la campaña se considera cumplida. 
cdFundedADA : Integer (datum field)
Cantidad total de ADA recaudada
cdCollectedADA : Integer (datum field)
Cantidad de ADA retirada por los administradores
cdBeginAt : Date (datum field)
Fecha de inicio de la campaña
cdDeadline : Date (datum field)
Fecha límite de la campaña
cdStatus : CampaignStatus (datum field)
Estado actual de la campaña en el datum
cdMilestones : [CampaignMilestones] (datum field)
Lista de hitos de la campaña en el datum
cdFundsCount : Integer (datum field)
Cantidad total de UTXOs de fondos
cdFundsIndex : Integer (datum field)
Índice para generar nuevos IDs de fondos
cdMinADA : Integer (datum field)
Cantidad mínima de ADA requerida en el UTXO
Description: String
LogoUrl: String
BannerUrl: String
Website: String
Instagram: String
Twitter: String
Discord: String
Facebook: String
Investors: Integer
Cantidad de inversores
Tokenomics-MaxSupply: String
Tokenomics-Description: String
Featured: Boolean
Para destacar en home
Archived: Boolean
Para no mostrar en el sitio web
CreatedAt: Date
UpdatedAt: Date

Campos dinámicos:
Project: Project
Campaign-Category: Campaign-Category
Campaign-Status: Campaign-Status
Creator-Wallet: Wallet
Members-Wallets: [Campaign-Member]
Campaign-Submission: [Campaign-Submission]
Campaign-Contents: [Campaign-Contents]
Milestones: [Milestone]
Milestone
Son los milestones de las campañas. Alguno de sus campos se sincronizan con el datum de la campaña que contiene el estado actual de los milestones en la red.
Milestone-ID: Integer 
Campaign-ID: Integer 
Campaign-Status-ID: Integer 
id del estado de campaña.
cmEstimatedDeliveryDate : Date (datum field)
Fecha estimada de entrega del milestone. 
cmPercentage : Integer (datum field)
Porcentaje sobre el total de los fondos ADA acumulados que representa este milestone
cmStatus : MilestoneStatus (datum field)
Estado actual del milestone en el datum, indica su progreso (Created, Success, Failed)
Description: String

Campos dinámicos:
Campaign: Campaign
Milestone-Status: Milestone-Status
Milestone-Submissions: [Milestone-Submission]
Wallet
Tabla interna del sistema que maneja las billeteras que se conectan.
ID-Wallet: Integer
createdBy: String
La lógica por la cúal se creó esta entrada
lastConnection: Date
walletUsed: String
walletValidatedWithSignedToken: Boolean
paymentPKH: String
stakePKH: String
name: String
email: String
ValidatedEmail: Boolean
El email de la billetera ha sido validado
testnet_address: String
mainnet_address: String
CreatedAt: Date
UpdatedAt: Date

Campos dinámicos:
Protocol-Admin: [Protocol]
Project-Creator: [Project]
Campaigns-Created: [Campaign]
Campaigns-Member: [Campaign-Member]
Campaigns-Submissions: [Campaign-Submission]
Milestone-Submissions: [Milestone-Submission]
Campaign-Member
Para relacionar la campaña y sus miembros.

ID-Campaign-Member: Integer
Campaign-ID: Integer
User-ID: Integer
Editor: Boolean
Para permitir la edición de la campaña en la DB
Wallet-ID: Integer
id del usuario 
Rol: String
Description: String
Website: String
Instagram: String
Twitter: String
Discord: String
Facebook: String
CreatedAt: Date
UpdatedAt: Date

Campos dinámicos:
Campaign: Campaign
Wallet: Wallet
Submission-Status
Los estados serían: submitted, approved, rejected y failed 
ID-Submission-Status: Integer
Name: String
Description: String
CreatedAt: Date
UpdatedAt: Date
Campaign-Submission
Cuando los usuarios envían la campaña para revisión
ID-Campaign-Submission: Integer
Campaign-ID: Integer
Submission-Status-ID: Integer 
SubmittedBy-Wallet-ID: Integer
RevisedBy-Wallet-ID: Integer
Approved-Justification: razón por la que se aprobó
Rejected-Justification: razón por la que se rechazó
CreatedAt: Date
UpdatedAt: Date

Campos dinámicos:
Campaign: Campaign
Submission-Status: Submission-Status
SubmittedBy-Wallet: Wallet
RevisedBy-Wallet: Wallet
Milestone-Submission
Cuando los usuarios envían el milestone para revisión
ID-Milestone-Submission: Integer
Milestone-ID: Integer
Submission-Status-ID: Integer 
SubmittedBy-Wallet-ID: Integer
RevisedBy-Wallet-ID: Integer
Report-Proof-of-FInalization: String
Approved-Justification: razón por la que se aprobó
Rejected-Justification: razón por la que se rechazó
CreatedAt: Date
UpdatedAt: Date

Campos dinámicos:
Campaign: Campaign
Milestone: Milestone
Submission-Status: Submission-Status
SubmittedBy-Wallet: Wallet
RevisedBy-Wallet: Wallet
SmartDB tablas internas
SmartDB mantiene varias tablas internas para su funcionamiento:
address_to_follow:
Almacena addresses a monitorear
Mapea tipos de datums esperados
Configura parámetros de sincronización
smart_utxo:
Registra UTXOs relevantes
Mantiene estado de locks
Optimiza selección para transacciones
site:
Configuraciones globales
Parámetros del sistema
Variables de entorno
job:
Registro de procesos en ejecución
Control de tareas programadas
Monitoreo de sincronización
token_metadata:
Caché de metadata de tokens
Integración con Blockfrost
Actualización periódica
transactions:
Lista de todas las transacciones realizadas en el website
Website (Frontend)
Wallet 
Con estos modales cubrimos toda la iteración que necesitamos de los usuarios.
¿Por qué no usamos Google Login?
Hemos decidido evitar el login con Google (o cualquier otro sistema de autenticación tradicional) por una razón de simplicidad arquitectónica:
Si permitiéramos que los usuarios se identifiquen tanto por email (Google) como por billetera, nos enfrentaríamos a escenarios complejos:
Un usuario podría loguearse primero con Google y luego conectar una billetera
O podría conectar primero la billetera y luego hacer login con Google
Necesitaríamos lógica adicional para vincular y mantener sincronizadas ambas identidades
Deberíamos manejar casos donde las vinculaciones podrían ser incorrectas o conflictivas
En su lugar, utilizamos únicamente la billetera como identificación primaria, y si necesitamos un email para comunicaciones, este se vincula directamente a la billetera. Esta aproximación es más simple, robusta y adecuada para una aplicación web3.
Modal Wallet Connector
El wallet connector tiene que permitir elegir las billeteras:
Debe mostrar la red de cardano que se está utilizando. 
Debe mostrar la lista de billeteras disponibles para conectar: eternl, yoroi, etc con Ícono y nombre.
Debe mostrar el Toggle button para loguearse con admin mode (Admin: Yes/No)
Botón Connect

Falta en el figma ese modal.

Referencia: 

En este ejemplo cada botón hace el connect con esa wallet.

Una vez conectado, en el header debe aparecer algo asi:



Ese botón tiene que abrir un nuevo modal con info de la billetera.
Modal Wallet Info
El modal Wallet Info tiene que mostrar: 
Billetera que se está usando (eternl, etc)
Togglebutton para loguearse con admin mode (Admin: Yes/No)
Address
Balance de ADA
Email registrado
Si el email está ingresado pero aún no validado debe mostrarse un mensaje de falta validar. Debe mostrarse en cualquier caso un botón Update o el mismo email puede ser el botón .
Si no hay email ingresado se muestra sólo el botón Update. 
Update abre Modal Wallet Email Update. 
Botón Disconnect para desconectar la billetera.

Referencia: 

Aquí falta lo del email y se muestra ese balance de MAYZ y Delegations que no hace falta.
En lugar del botón Portfolio podría ser un botón para ir al listado de campañas creadas o administradas por el usuario. Nos lleva a lo que hoy es la página Drafts que yo le cambiaría el nombre por Manage. El botón podría llamarse Manage Proposals.
Podría también estar el botón para ver las campañas en donde el usuario invirtió. El botón podría llamarse My Invest. Nos lleva al Home con el check en un nuevo toggle button que propongo llamar My Invest. 
Modal Wallet Email Update
Un nuevo modal debe permitir ingresar un email en un campo de texto. Debe mostrar:
Campo de texto con el email ingresado o en blanco si no hay email aún.
Botón Update y Cancel (o back)
Si el usuario ya ingresó un email antes, debe aparecer el botón Unlink. 
Si el usuario ya ingresó un email antes, pero aún falta validarlo debe aparecer el botón Validate.
Botón Update y VALIDATE nos lleva a la pantalla de Wallet Validate Email.
Botón Cancel lo lleva atrás al modal de Wallet Info
Botón Unlink pregunta en un nuevo modal Unlink al usuario con botones Accept or Cancel. Accept lo lleva a el modal de Wallet Info. Cancel lo lleva al modal de Wallet Email Update.

De existir otro usuario con el email ingresado y validado: no permitir ingresar ese email o simplemente se saca ese email de esa otra billetera y se agrega a este usuario. 

Referencia:
YOUR EMAIL
Enter your email (o el email anteriormente ingresado)
VALIDATE | UPDATE | CANCEL | UNLINK


Referencia modal Unlink
YOUR EMAIL
email ingresado
Do you want to unlink this email?
ACCEPT | CANCEL 


Modal Wallet Validate Email
YOUR EMAIL
email ingresado
An email was sent to your mailbox. Follow the link to validate your email.
CLOSE


Se va a generar un link de validación que el usuario deberá abrir.
Una vez validado siguiendo el link en el email recibido, quedará vinculada esa billetera con ese email. 
Email de validación:
INNOVATIO HEADER
Follow this link to validate you email and connect it with your wallet address: address
FOOTER

Home
Listado de campañas
Aparece el listado de campañas con las que los usuarios inversores pueden interactuar. Este listado se puede filtrar por categoría, estado y campo de texto.

El botón de Explore More campaign podría abrir una página similar a Home, pero más enfocada en mostrar más campañas por página?

El listado que se presenta va a variar de acuerdo a si los usuarios están conectados o no, o sí el usuario conectado es administrador de algunas campañas.

Para los usuarios que no se conectan, 

El listado incluye todas las campañas en los siguientes estados:
Countdown
Fundraising
Finishing
Active
Success
Failed
Unreached
No se muestran las campañas TBL.

Si el usuario conectó la billetera y está en admin mode el listado incluye también todas las campañas que el usuario puede administrar sin importar el estado. Si la billetera conectada es una billetera administradora del protocolo, se van a mostrar todas las campañas en todos .

Si el usuario conectó la billetera debe aparecer el toggle button o check de My Proposal. 


El listado que se va a presentar son todas aquellas campañas que el usuario creó con esa billetera o si su billetera es administrador de alguna campaña 
Si tiene un email vinculado y validado y este email está registrado como el creador o editor de algunas campañas también estas aparecerán
Si el usuario compró tokens de alguna campaña
Para ver todas las campañas del usuario incluyendo las TBL (To be Launched) que no se muestran en Home, se deberá ingresar a la página de Manage
Items de campaña 
Van a cambiar de acuerdo al estado
Cada estado muestra botones diferentes
El título e imagen nos llevan a la Página del Detalle de Campaña
Si el usuario está conectado en admin mode y es administrador de la campaña, todas las campañas de usuario van a mostrar los mismos botones que se muestran para esa campaña en la página Manage.
Si el usuario no está conectado o no está en admin mode o no es admin de esa campaña se muestran los botones que se detallan a continuación
Botones
Created: No se muestran en home
Submitted: No se muestran en home
Approved: No se muestran en home
Contract Created: No se muestran en home
Contract Published: No se muestran en home
Contract Started: No se muestran en home
Countdown: 

Fundraising:

Finishing: 

Es similar a Fundraising, pero sólo se muestra botón Learn More
Active: 

Success:

Failed: 

Falta agregar botón para que los usuarios devuelvan los tokens y recuperen ADA
Unreached: 

Falta agregar botón para que los usuarios devuelvan los tokens y recuperen ADA

Drafts & Manage
Página que muestra todas las propuestas que están preparándose, o que podemos administrar. Todas las campañas tienen una serie de acciones que se pueden hacer. 

A esta página debe de accederse mediante algún botón y por ahora no encuentro en el Figma ese acceso. Mis propuestas son:
En el Modal Wallet Info, con un Botón de Manage Proposals
En el Header, al lado del Botón de Create New Campaign

Estos botones pueden / deben aparecer sólo si el usuario está conectado en admin mode.

Si se ingresa a esta página y el usuario no está conectado o no está conectado en admin mode, debe aparecer un mensaje de error solicitando al usuario conectar su wallet en admin mode.
Listado de Campañas
El listado aquí presentado:

El listado que se va a presentar son todas aquellas campañas que el usuario creó con esa billetera o si su billetera es administrador de alguna campaña (las campañas tendrán una lista de billeteras que podrán hacer cambios en ella). 
Estos usuarios podrán actualizar la campaña en la base de datos y también podrán operar con la parte on-chain de la campaña.
Si tiene un email vinculado y validado y este email está registrado como el creador o editor de algunas campañas también estas aparecerán: pero sólo podrá actualizar datos de la campaña que se guardan en la base de datos, no podrá hacer transacciones para administrar la campaña on-chain.
Botones
Created: determinar botones que se muestran en tarjeta
Submitted: determinar botones que se muestran en tarjeta
Rejected: determinar botones que se muestran en tarjeta
Approved: determinar botones que se muestran en tarjeta
Contract Created: determinar botones que se muestran en tarjeta
Contract Published: determinar botones que se muestran en tarjeta
Contract Started: determinar botones que se muestran en tarjeta
Countdown: determinar botones que se muestran en tarjeta
Fundraising: determinar botones que se muestran en tarjeta
Finishing:  determinar botones que se muestran en tarjeta
Active: determinar botones que se muestran en la tarjeta. Recordar que estando en active la campaña, los botones van a cambiar según el estado del milestone en desarrollo. Estados de los milestones son:
Not Started: 
Started: 
Submitted: 
Rejected: 
Finished: 
Failed: 
Success: determinar botones que se muestran en tarjeta
Failed: determinar botones que se muestran en tarjeta
Unreached: determinar botones que se muestran en tarjeta
